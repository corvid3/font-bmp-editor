#include <algorithm>
#include <chrono>
#include <cmath>
#include <concepts>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <ftxui/component/component.hpp>
#include <ftxui/component/component_base.hpp>
#include <ftxui/component/component_options.hpp>
#include <ftxui/component/event.hpp>
#include <ftxui/component/loop.hpp>
#include <ftxui/component/screen_interactive.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/screen/color.hpp>
#include <ftxui/screen/screen.hpp>
#include <ftxui/screen/terminal.hpp>
#include <functional>
#include <ios>
#include <iterator>
#include <optional>
#include <ostream>
#include <span>
#include <stdexcept>
#include <string_view>
#include <thread>
#include <vector>

template<typename T>
class OStreamMaybe
{
  std::unique_ptr<T> m_maybe;

public:
  OStreamMaybe()
    : m_maybe(nullptr) {};
  OStreamMaybe(T* owning)
    : m_maybe(owning) {};

  void reset(T* owning = nullptr) { m_maybe.reset(owning); }

  template<typename O>
  T& operator<<(O arg)
  {
    if (m_maybe != nullptr)
      *m_maybe << arg;

    return *m_maybe;
  }
};
// acts monadically
class LogFile
{
public:
  enum class Level
  {
    Info,
    Error,
  };

private:
  std::unique_ptr<std::ostream> m_stream;
  std::optional<Level> m_currentLevel;
  std::string_view get_level_text()
  {
    if (!m_currentLevel)
      return "";
    switch (*m_currentLevel) {
      case Level::Info:
        return "INFO  :: ";
      case Level::Error:
        return "ERROR :: ";
    }

    fprintf(stderr, "mysteriously fell out of switch in get_level_text");
    exit(1);
  }

public:
  LogFile()
    : m_stream(nullptr)
  {
  }

  LogFile(std::filesystem::path const& path)
    : m_stream(new std::ofstream(path, std::ios_base::app | std::ios_base::out))
  {
  }

  LogFile& operator<<(Level const level)
  {
    m_currentLevel = level;
    return *this;
  }

  LogFile& operator<<(std::string_view what)
  {
    if (m_stream) {
      auto const zone = std::chrono::current_zone();
      auto const now = std::chrono::system_clock::now();

      *m_stream << std::format(
                     "[{:%F %X}] {}", zone->to_local(now), get_level_text())
                << what << std::endl;

      m_currentLevel.reset();
    }

    return *this;
  }
};

class logged_runtime_error : public std::runtime_error
{
public:
  logged_runtime_error(OStreamMaybe<LogFile>& log,
                       std::convertible_to<std::string_view> auto const& msg)
    : runtime_error(msg)
  {
    // FIXME: this causes a segfault??
    // happens in the ostreammaybe "== nullptr" if check. what.
    // log << LogFile::Level::Error << msg;
  }
};

// this is a global because... why not. should probably just store
// it in the context struct though
OStreamMaybe<LogFile> g_logFile;

// TODO: potentially add resizing...
// but if you're going to resize
// the font might as well completely start over lol
std::vector<unsigned char>
readFile(std::filesystem::path const& path)
{
  std::ifstream i(path, std::ios::in | std::ios::binary);

  if (i.fail())
    throw logged_runtime_error(
      g_logFile,
      std::format("unable to open file: {} doesn't exist",
                  path.generic_string()));

  using namespace std;
  vector<unsigned char> out;
  copy(istreambuf_iterator<char>(i),
       istreambuf_iterator<char>(),
       inserter(out, out.end()));

  g_logFile << std::format(
    "FILESYSTEM: {}, VECTOR: {}", filesystem::file_size(path), out.size());
  return out;
}

template<typename T, auto const SIZE>
concept BitWidth = requires { requires sizeof(T) == SIZE; };

template<typename T>
  requires std::input_iterator<T> &&
           std::is_convertible_v<typename std::iterator_traits<T>::value_type,
                                 char>
class SlideBuf
{
  T m_begin;
  T m_end;

public:
  SlideBuf(T const& in, T const& end)
    : m_begin(in)
    , m_end(end)
  {
  }

  operator bool() { return m_begin != m_end; }

  auto len() const -> std::integral auto { return m_end - m_begin; }

  template<typename O>
    requires BitWidth<O, 1>
  void operator>>(O& out)
  {
    if (m_begin == m_end)
      throw std::runtime_error("ran out of end of slidebuf range | slide 1");

    out = *m_begin;
    m_begin += 1;
  }

  template<typename O>
    requires BitWidth<O, 2>
  void operator>>(O& out)
  {
    if (m_begin + 1 >= m_end)
      throw std::runtime_error("ran out of end of slidebuf range | slide 2");

    char tmpl, tmpr;
    *this >> tmpl;
    *this >> tmpr;
    out = (static_cast<O>(tmpl) << 8) | (tmpr & 0xFF);
  }

  void operator>>(std::convertible_to<std::string> auto& t)
  {
    char c;
    (*this) >> c;
    t.push_back(c);
  }
};

template<typename T>
constexpr auto
getSize()
{
  return sizeof(T);
}

class byte_stream
{
  std::vector<char> m_data;

public:
  template<typename T>
    requires std::integral<T> and BitWidth<T, 1>
  void operator<<(T const in)
  {
    m_data.push_back(in);
  }

  template<typename T>
    requires std::integral<T> and BitWidth<T, 2>
  void operator<<(T const in)
  {
    m_data.push_back(in >> 8 & 0xFF);
    m_data.push_back(in & 0xFF);
  }

  template<typename T>
    requires std::is_convertible_v<T, std::string_view>
  void operator<<(T const& in)
  {
    std::string_view instr(in);
    m_data.insert(m_data.end(), instr.begin(), instr.end());
  }
  template<typename T>
    requires requires(T i) { std::span(i); }
  void operator<<(T const& in)
  {
    std::span sp(in);
    m_data.insert(m_data.end(), sp.begin(), sp.end());
  }
  std::vector<char> eat() && { return std::move(m_data); }
};

// may be worth it to not unpack the glyph data
// in the FontData struct, but only
// unpack in the workspace, uses up less memory
struct FontData
{
  int m_height, m_width;
  std::vector<std::vector<bool>> m_data;

  FontData(const FontData&) = delete;
  FontData(FontData&&) = default;
  FontData& operator=(const FontData&) = delete;
  FontData& operator=(FontData&&) = delete;

  FontData(unsigned const height,
           unsigned const width,
           unsigned const num = 256)
    : m_height(height)
    , m_width(width)
  {
    for (unsigned i = 0; i < num; i++)
      m_data.push_back(std::vector<bool>(height * width));
  }

  FontData(std::filesystem::path const& path)
  {
    auto const data = readFile(path);
    SlideBuf buf(data.begin(), data.end());

    std::string magic;

    for (auto i = 0; i < 3; i++)
      buf >> magic;

    if (magic != "BMP")
      throw logged_runtime_error(
        g_logFile,
        std::format("invalid magic at beginning of fontdata file, found: {}",
                    magic));

    std::string version;
    for (auto i = 0; i < 3; i++)
      buf >> version;
    if (version != "000")
      throw logged_runtime_error(
        g_logFile,
        std::format(
          "currently only able to decode version 000 of the spec, found: {}",
          version));

    unsigned char width;
    unsigned char height;
    unsigned short num;

    buf >> width;
    buf >> height;
    buf >> num;

    if (width == 0)
      throw logged_runtime_error(g_logFile, "width is somehow 0, wtf!");
    if (height == 0)
      throw logged_runtime_error(g_logFile, "height is somehow 0, wtf!");
    if (num == 0)
      throw logged_runtime_error(g_logFile, "num is somehow 0, wtf!");

    g_logFile << LogFile::Level::Info
              << std::format(
                   "loading font [width: {}, height: {}, num: {}] bytes: {}",
                   width,
                   height,
                   num,
                   data.size());

    m_width = width;
    m_height = height;

    unsigned const bits_per_glyph = width * height;
    unsigned const bytes_per_glyph = (bits_per_glyph + 7) / 8;

    std::vector<char> workspace(bytes_per_glyph);

    auto lookup = [&](unsigned bit) {
      unsigned const byte_idx = bit / 8;
      unsigned char const ch = workspace.at(byte_idx);
      unsigned char const slice = 1 << 7;
      unsigned char const amt = bit % 8;
      return ch & (slice >> amt);
    };

    for (unsigned g = 0; g < num; g++) {
      unsigned b = 0;
      try {
        for (b = 0; b < bytes_per_glyph; b++)
          buf >> workspace.at(b);

        std::vector<bool> glyph_data;

        for (unsigned b = 0; b < bits_per_glyph; b++) {
          bool const bit = lookup(b);
          glyph_data.push_back(bit);
        }

        m_data.push_back(glyph_data);
      } catch (std::exception const& e) {
        throw logged_runtime_error(
          g_logFile, std::format("{} @ letter {}", e.what(), (int)g));
      }
    }

    // lets do a couple of sanity checks...
    if (bits_per_glyph != m_data[0].size())
      throw logged_runtime_error(
        g_logFile,
        "a glyph does not countain the amount of bools per bits in a glyph!");
    if (m_data.size() != num)
      throw logged_runtime_error(g_logFile, "num of glyphs != num!");
  }

  // TODO: actually encode the data...
  std::vector<char> Encode() const
  {
    byte_stream stream;

    // get the header out of the way
    stream << "BMP";
    // what version we're using...
    stream << "000";
    stream << (char)m_width;
    stream << (char)m_height;
    stream << (short)m_data.size();

    auto const bits_per_glyph = m_width * m_height;
    auto const bytes_per_glyph = (bits_per_glyph + 7) / 8;

    for (auto& glyph : m_data) {
      std::vector<unsigned char> glyphBits(bytes_per_glyph, 0);

      auto setBitTrue = [&](unsigned idx) {
        auto const bytes = idx / 8;
        unsigned char& c = glyphBits.at(bytes);

        unsigned char bit_tmp = 1 << 7;
        bit_tmp >>= idx % 8;
        c &= ~bit_tmp;
        c |= bit_tmp;
      };

      for (int h = 0; h < m_height; h++)
        for (int w = 0; w < m_width; w++)
          if (glyph.at(h * m_width + w))
            setBitTrue(h * m_width + w);

      stream << glyphBits;
    }

    return std::move(stream).eat();
  }
};

struct GlyphData
{
  std::vector<unsigned char> m_data;
};

struct Cursor
{
  unsigned m_x{}, m_y{};

  void constrain(unsigned const width, unsigned const height)
  {
    m_x = std::min<unsigned>(m_x, width - 1);
    m_y = std::min<unsigned>(m_y, height - 1);
  }

  void down() { m_y += 1; }
  void up()
  {
    if (m_y != 0)
      m_y -= 1;
  }
  void left()
  {
    if (m_x != 0)
      m_x -= 1;
  }
  void right() { m_x += 1; }
};

class Workspace
{
  FontData m_font;
  std::vector<bool> m_workingSpace;
  int m_selectedGlyph = 'A';

public:
  Workspace(FontData&& font)
    : m_font(std::move(font))
    , m_workingSpace(m_font.m_height * m_font.m_width, false)
    , m_selectedGlyph(' ')
  {
    // make SURE to load the currently selected glyph
    // into the workspace, or it'll be
    // unintentionally overwritten o_o
    m_workingSpace = m_font.m_data.at(m_selectedGlyph);
  }

  std::vector<bool>& getWorkingValues() { return m_workingSpace; }
  Cursor m_curs;

  FontData const& getFontData() const { return m_font; }
  unsigned width() const { return m_font.m_width; }
  unsigned height() const { return m_font.m_height; }

  unsigned currentGlyph() const { return m_selectedGlyph; }
  unsigned numGlyphs() const { return m_font.m_data.size(); }

  void loadNextGlyph()
  {
    if (m_font.m_data.size() != 0 and
        m_selectedGlyph < (int)m_font.m_data.size() - 1)
      loadGlyph(m_selectedGlyph + 1);
  }

  void loadPrevGlyph()
  {
    if (m_selectedGlyph > 0)
      loadGlyph(m_selectedGlyph - 1);
  }

  void saveCurrentGlyph()
  {
    auto& glyph = m_font.m_data[m_selectedGlyph];
    glyph = m_workingSpace;
  };

  void loadGlyph(unsigned const glyphNo)
  {
    if (glyphNo >= numGlyphs())
      throw logged_runtime_error(g_logFile,
                                 "trying to load glyph outside of valid range");

    saveCurrentGlyph();
    m_selectedGlyph = glyphNo;
    std::vector<bool>& glyph = m_font.m_data[glyphNo];
    m_workingSpace = decltype(m_workingSpace)(glyph.begin(), glyph.end());
  }
};

struct Context
{
  std::optional<Workspace> m_workspace;

  std::string m_errorMsg;
  bool m_modalError{ false };
  bool m_modalLoad{ false };
  bool m_modalSave{ false };
  bool m_modalNew{ false };

  // TODO: make this a stack maybe?
  void error(std::string const& what)
  {
    m_errorMsg = what;
    m_modalError = true;
  }
};

ftxui::Element
glyphRenderer(Context& ctx)
{
  using namespace ftxui;

  if (!ctx.m_workspace.has_value())
    return text("no font loaded");

  auto& ws = *ctx.m_workspace;

  auto const height = ws.height();
  auto const width = ws.width();

  std::function<bool(int, int)> is_enabled = [&](auto const w, auto const h) {
    return ws.getWorkingValues()[h * width + w];
  };

  std::vector<Element> lines;
  for (unsigned h = 0; h < height; h++) {
    std::string line;
    for (unsigned w = 0; w < width; w++) {
      auto const en = is_enabled(w, h);

      if ((ws.m_curs.m_x == w xor ws.m_curs.m_y == h) && !en) {
        line.append("░");
      } else if (ws.m_curs.m_x == w && ws.m_curs.m_y == h) {
        if (en)
          line.append("▓");
        else
          line.append("X");
      } else {
        if (en)
          line.append("█");
        else
          line.append(" ");
      };
    }
    lines.push_back(text(line));
  }
  return select(vbox(lines));
}

ftxui::Component
mainWorkspace(Context& ctx)
{
  using namespace ftxui;

  auto main_event_catcher = CatchEvent([&](Event e) {
    if (!ctx.m_workspace)
      return false;
    auto& ws = *ctx.m_workspace;

    bool to_leave = true;

    if (e == e.ArrowUp && ws.m_curs.m_y == 0)
      to_leave = false;

    if (e == e.ArrowDown)
      ws.m_curs.down();
    if (e == e.ArrowUp)
      ws.m_curs.up();
    if (e == e.ArrowLeft)
      ws.m_curs.left();
    if (e == e.ArrowRight)
      ws.m_curs.right();

    if (e == e.PageUp)
      ws.loadNextGlyph();
    if (e == e.PageDown)
      ws.loadPrevGlyph();

    if (e == Event::Character(' ')) {
      auto v = ws.getWorkingValues().begin() + ws.m_curs.m_y * ws.width() +
               ws.m_curs.m_x;

      *v = !*v;

      ws.saveCurrentGlyph();
    }

    ws.m_curs.constrain(ws.width(), ws.height());

    return to_leave;
  });

  auto title = Renderer([&] {
                 if (!ctx.m_workspace)
                   return text("");
                 auto& ws = *ctx.m_workspace;

                 auto const c = ws.currentGlyph();

                 // TODO: actually show the currently selected character
                 return text(std::format(
                   "{}:{} | {:X} '{}' | {} chars of {}x{}",
                   ws.m_curs.m_x,
                   ws.m_curs.m_y,
                   c,
                   std::isprint(c) ? std::format("{}", (char)c) : "ND",
                   ws.getFontData().m_data.size(),
                   ws.width(),
                   ws.height()));
               }) |
               border | center | flex_shrink;

  // █
  auto main_workspace =
    Container::Stacked({ Input() | main_event_catcher,
                         Renderer([&] { return glyphRenderer(ctx); }) }) |
    border | flex_shrink | center;

  auto main_component = Container::Vertical({ title, main_workspace });

  return main_component;
}

struct Arguments
{
  std::optional<std::string> m_loadPath;
  std::optional<std::string> m_logPath;

  Arguments(int argc, char** argv)
  {
    int c = 0;
    while ((c = getopt(argc, argv, "F:L:")) != -1) {
      switch (c) {
        case 'F':
          m_loadPath = ::optarg;
          break;
        case 'L':
          m_logPath = ::optarg;
          break;

        default:
          throw logged_runtime_error(
            g_logFile,
            "unknown argument found while trying to parse launch args");
      }
    };
  }
};

int
main(int argc, char** argv)
{
  using namespace ftxui;
  Arguments args(argc, argv);

  Context ctx;

  if (args.m_loadPath) {
    auto const& path = *args.m_loadPath;
    ctx.m_workspace.emplace(FontData(path));
  }

  if (args.m_logPath) {
    auto const& path = *args.m_logPath;
    g_logFile.reset(new LogFile(path));
  }

  auto screen = ScreenInteractive::Fullscreen();
  auto exit = screen.ExitLoopClosure();
  auto width_component = Container::Horizontal({});

  auto constexpr height = 8;
  auto constexpr width = 8;

  std::vector<bool> working_space(width * height);

  auto help =
    Renderer([&] {
      return vbox(
        { hbox({ text("F1 to exit") | border | color(Color::Red),
                 text("F2 to save") | border | color(Color::Green),
                 text("F3 to load") | border | color(Color::Blue) }),
          hbox({ text("F4 to create") | border | color(Color::GreenYellow),
                 text("F5 to unload") | border | color(Color::Red),
                 text("F6 to change num") | border | color(Color::Green) }) });
    }) |
    center;

  auto main_workspace = mainWorkspace(ctx);

  std::string load_input_str;
  auto load_input =
    Input(&load_input_str, "filepath") |
    CatchEvent([&](Event e) { return e.character()[0] == '\n'; });

  auto load_dialog =
    Modal(Container::Vertical(
            { Renderer(load_input,
                       [&] {
                         return vbox({ text("LOAD FONT") | border | center,
                                       load_input->Render() | border });
                       }),
              Container::Horizontal(
                { Button("Ok",
                         [&] {
                           try {
                             ctx.m_workspace.reset();
                             ctx.m_workspace.emplace(FontData(load_input_str));
                           } catch (std::exception const& e) {
                             ctx.error(std::format("unable to load file: {}",
                                                   e.what()));
                           }
                           ctx.m_modalLoad = false;
                         }),
                  Button("Cancel", [&] { ctx.m_modalLoad = false; }) }) }) |
            size(ftxui::WIDTH, Constraint::GREATER_THAN, 20) | border,
          &ctx.m_modalLoad);

  auto ignore_enter =
    CatchEvent([&](Event e) { return e.character()[0] == '\n'; });

  std::string save_input_str;
  auto save_input = Input(&save_input_str, "filepath") | ignore_enter;

  auto save_dialog =
    Modal(Container::Vertical(
            { Renderer(save_input,
                       [&] {
                         return vbox({ text("SAVE FONT") | border | center,
                                       save_input->Render() | border });
                       }),
              Container::Horizontal(
                { Button("Ok",
                         [&] {
                           try {
                             auto out = ctx.m_workspace->getFontData().Encode();
                             std::ofstream outfile(save_input_str);
                             outfile.write(out.data(), out.size());

                           } catch (std::exception const& e) {
                             ctx.error(std::format("unable to save file: {}",
                                                   e.what()));
                           }
                           ctx.m_modalSave = false;
                         }),
                  Button("Cancel", [&] { ctx.m_modalSave = false; }) }) }) |
            size(ftxui::WIDTH, Constraint::GREATER_THAN, 20) | border,
          &ctx.m_modalSave);

  std::string new_dialog_width;
  std::string new_dialog_height;
  auto new_dialog_width_input =
    Input(&new_dialog_width, "width") | ignore_enter;
  auto new_dialog_height_input =
    Input(&new_dialog_height, "height") | ignore_enter;
  auto new_dialog =
    Modal(Container::Vertical(
            { Container::Horizontal({ new_dialog_width_input | border,
                                      new_dialog_height_input | border }),
              Container::Horizontal(
                { Button("OK",
                         [&] {
                           ctx.m_modalNew = false;
                           ctx.m_workspace.emplace(
                             FontData(std::stoi(new_dialog_width),
                                      std::stoi(new_dialog_height),
                                      256));
                         }),
                  Button("CANCEL", [&] { ctx.m_modalNew = false; }) }) }) |
            border,
          &ctx.m_modalNew);

  auto error_dialog =
    Modal(Container::Vertical(
            { Renderer([&] {
                return vbox(
                  text("an error has occurred: ") | border | center,
                  text(ctx.m_errorMsg) | center | color(Color::Red) |
                    size(WidthOrHeight::HEIGHT, Constraint::GREATER_THAN, 3));
              }),
              Button("Ok",
                     [&] {
                       ctx.m_modalError = false;
                       ctx.m_errorMsg.clear();
                     }) }) |
            border,
          &ctx.m_modalError);

  auto window_component =
    Container::Vertical({ help, main_workspace }) |
    CatchEvent([&exit, &ctx](Event event) {
      if (event == event.F1)
        return exit(), true;

      // saving
      if (event == event.F2) {
        if (!ctx.m_workspace)
          ctx.error("cannot save if there is no font loaded!");
        else {
          ctx.m_modalSave = true;
          // ctx.error("saving not implemented yet");
        }
      }

      // load
      if (event == event.F3) {
        if (ctx.m_workspace)
          ctx.error(
            "close the current font before attempting to load another!");
        else {
          ctx.m_modalLoad = true;
        }
      }

      if (event == event.F4) {
        if (ctx.m_workspace) {
          ctx.error("you must unload the current font to create another!");
        } else {
          // TODO: make a popup that lets you set the font size
          ctx.m_modalNew = true;
        }
      }

      if (event == event.F5) {
        // TODO: make a popup "are you sure?" box
        if (!ctx.m_workspace) {
          ctx.error("no font loaded to clear!");
        } else {
          ctx.m_workspace.reset();
        }
      }
      return false;
    }) |
    load_dialog | save_dialog | new_dialog | error_dialog;

  g_logFile << std::format("bmp editor loaded, starting loop");

  // it may genuinely be worth just looping every 16ms and
  // just constantly copying the data from the current
  // data into the glyph every frame... it's only like 512b at most!
  Loop loop(&screen, window_component);
  while (not loop.HasQuitted()) {
    loop.RunOnce();
    std::this_thread::sleep_for(std::chrono::milliseconds(16));
  }
}
