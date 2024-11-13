use imgui::{
    Id, ProgressBar, SelectableFlags, TableColumnFlags, TableColumnSetup, TableFlags,
    TableSortDirection, TreeNodeFlags,
};
use std::{
    cmp::Ordering,
    env,
    ffi::{CString, OsStr},
    fmt::Display,
    fs::{read_dir, File},
    io::Read,
    num::{ParseFloatError, ParseIntError},
    path::{Path, PathBuf},
    str::Lines,
    sync::{Arc, Mutex},
};

mod boilerplate_sdl2;

fn main() {
    let mut windows: Vec<(bool, Window)> = Vec::new();

    // There can be duplicate window titles so we add invisible counter
    let mut next_window_no = 0;
    let mut init_windows = Vec::new();
    let mut args = env::args().into_iter();
    let _prog = args.next();
    for arg in args {
        if arg == "-h" || arg == "--help" {
            eprintln!("Usage: imghcprof [FILES...]");
            std::process::exit(1);
        }

        let new = NewWindow::Profile {
            input_file: arg.into(),
        };
        init_windows.push(new);
    }

    // Windows request new windows via this mutex
    let new_window: Arc<Mutex<Option<NewWindow>>> = Arc::new(Mutex::new(None));

    let mut file_picker = FilePicker::new(env::current_dir().unwrap_or("/".into()), |path| {
        path.file_name()
            .is_some_and(|f| f.to_str().is_some_and(|s| s.ends_with(".prof")))
    });
    let mut selecting_file = false;

    boilerplate_sdl2::draw("imghcprof", |ui| {
        // Create main tab with profiler window
        let dockspace = unsafe {
            let dockspace = imgui_sys::igDockSpaceOverViewport(
                imgui_sys::igGetMainViewport(),
                imgui_sys::ImGuiDockNodeFlags_PassthruCentralNode as i32,
                core::ptr::null(),
            );

            dockspace
        };

        for init_window in init_windows.drain(..) {
            add_new_window(init_window, &mut windows, &mut next_window_no, dockspace);
        }

        if let Some(_main_menu) = ui.begin_main_menu_bar() {
            if let Some(_new_menu) = ui.begin_menu("New") {
                selecting_file = true;
            }
        }

        if selecting_file {
            unsafe {
                imgui_sys::igSetNextWindowSize(
                    imgui_sys::ImVec2 { x: 300.0, y: 500.0 },
                    imgui_sys::ImGuiCond_Appearing as i32,
                );
                imgui_sys::igSetNextWindowPos(
                    imgui_sys::ImVec2 { x: 50.0, y: 50.0 },
                    imgui_sys::ImGuiCond_Appearing as i32,
                    imgui_sys::ImVec2 { x: 0.0, y: 0.0 },
                );
            }

            ui.open_popup("Select File");
            if let Some(_t) = ui.begin_modal_popup("Select File") {
                if let Some(picked) = file_picker.draw(ui) {
                    if let Some(picked) = picked {
                        *new_window.lock().unwrap() =
                            Some(NewWindow::Profile { input_file: picked });
                    }
                    file_picker.picked = None;
                    selecting_file = false;
                }
            }
        }

        for (is_opened, window) in &mut windows {
            ui.window(&window.title)
                .menu_bar(false)
                .collapsible(false)
                .title_bar(true)
                .opened(is_opened)
                .resizable(true)
                .build(|| {
                    draw_cost_tree(&ui, &mut window.tree, new_window.clone(), window.root_id);
                });
        }

        windows.retain(|(is_opened, _)| *is_opened);

        if let Some(new) = new_window.lock().unwrap().take() {
            add_new_window(new, &mut windows, &mut next_window_no, dockspace);
        }
    });
}

fn add_new_window(
    new: NewWindow,
    windows: &mut Vec<(bool, Window)>,
    next_window_no: &mut u32,
    dockspace: imgui_sys::ImGuiID,
) {
    match new {
        NewWindow::IncomingCalls {
            module,
            source,
            name,
            root_id,
        } => {
            let new_tree =
                CostTree::find(&windows[root_id as usize].1.tree, &module, &source, &name);
            let title = format!(
                "Calls to `{}` (`{}`)## {}",
                name, windows[root_id as usize].1.fname, next_window_no
            );
            let ctitle = CString::new(title.as_bytes()).unwrap();
            unsafe {
                imgui_sys::igDockBuilderDockWindow(ctitle.as_ptr().cast(), dockspace);
            }
            windows.push((
                true,
                Window {
                    title,
                    tree: new_tree,
                    root_id,
                    fname: windows[root_id as usize].1.fname.clone(),
                },
            ));
            *next_window_no += 1;
        }
        NewWindow::Focus {
            name,
            cost_center,
            root_id,
        } => {
            if let Some(new_tree) = CostTree::focus(&windows[root_id as usize].1.tree, cost_center)
            {
                let title = format!(
                    "Focus on `{}` (`{}`)## {}",
                    name, windows[root_id as usize].1.fname, next_window_no
                );
                let ctitle = CString::new(title.as_bytes()).unwrap();
                unsafe {
                    imgui_sys::igDockBuilderDockWindow(ctitle.as_ptr().cast(), dockspace);
                }
                windows.push((
                    true,
                    Window {
                        title,
                        tree: new_tree,
                        root_id,
                        fname: windows[root_id as usize].1.fname.clone(),
                    },
                ));
                *next_window_no += 1;
            }
        }
        NewWindow::Profile { input_file } => {
            let mk_tree = || -> Option<CostTree> {
                let mut input = String::new();
                let mut f = File::open(&input_file)
                    .map_err(|err| {
                        eprintln!("imghcprof: Could not open input file '{input_file:?}': {err}");
                        ()
                    })
                    .ok()?;
                f.read_to_string(&mut input)
                    .map_err(|err| {
                        eprintln!("imghcprof: Could not read input file '{input_file:?}': {err}");
                        ()
                    })
                    .ok()?;

                let mut parser = Parser::new(&input)
                    .map_err(|err| {
                        eprintln!("imghcprof: Could not parse input file '{input_file:?}': {err}");
                        ()
                    })
                    .ok()?;
                let mut tree = parser
                    .parse_tree()
                    .map_err(|err| {
                        eprintln!("imghcprof: Could not parse input file '{input_file:?}': {err}");
                        ()
                    })
                    .ok()?;
                tree.open_interesting();
                Some(tree)
            };

            if let Some(tree) = mk_tree() {
                let fname = input_file
                    .file_name()
                    .unwrap_or(OsStr::new("?"))
                    .to_str()
                    .unwrap_or("?");

                let title = format!("Profile `{fname}`## {next_window_no}");
                let ctitle = CString::new(title.as_bytes()).unwrap();
                unsafe {
                    imgui_sys::igDockBuilderDockWindow(ctitle.as_ptr().cast(), dockspace);
                }
                windows.push((
                    true,
                    Window {
                        title,
                        tree,
                        root_id: *next_window_no,
                        fname: fname.to_owned(),
                    },
                ));
                *next_window_no += 1;
            }
        }
    }
}

struct FilePicker<F> {
    picked: Option<PathBuf>,
    current_dir: PathBuf,
    show_all: bool,
    filter: F,
}

impl<F> FilePicker<F>
where
    F: Fn(&Path) -> bool,
{
    fn new(initial_path: PathBuf, filter: F) -> Self {
        Self {
            picked: None,
            current_dir: initial_path,
            show_all: false,
            filter,
        }
    }

    fn draw(&mut self, ui: &imgui::Ui) -> Option<Option<PathBuf>> {
        // TODO: Cache fs results to not spam the kernel/allocator too mutch
        // but this is rarely on screen so it's fine for now

        let mut ancestors = self.current_dir.ancestors().into_iter();
        let _ = ancestors.next();
        let up = ancestors.next();

        if ui.button("Up") {
            if let Some(d) = up {
                self.current_dir = d.to_owned();
                self.picked = None;
                return None;
            }
        }

        ui.same_line();
        let mut selected = false;
        ui.disabled(self.picked.is_none(), || selected = ui.button("Open"));
        ui.same_line();
        ui.checkbox("Show all", &mut self.show_all);
        ui.same_line();
        if ui.button("Cancel") {
            return Some(None);
        }

        macro_rules! get_name {
            ($f:expr) => {
                $f.path()
                    .file_name()
                    .unwrap_or(OsStr::new(""))
                    .to_str()
                    .unwrap_or("<?>")
            };
        }

        if let Some(_t) = ui
            .tree_node_config(self.current_dir.as_path().to_str().unwrap_or("???"))
            .open_on_arrow(false)
            .open_on_double_click(false)
            .opened(true, imgui::Condition::Always)
            .push()
        {
            if ui.is_item_clicked() {
                if let Some(d) = up {
                    self.current_dir = d.to_owned();
                    self.picked = None;
                }
            }

            let Ok(dir_iter) = read_dir(&self.current_dir) else {
                return selected.then_some(self.picked.clone());
            };
            let Ok(mut dir_entries) = dir_iter.collect::<Result<Vec<_>, _>>() else {
                return selected.then_some(self.picked.clone());
            };

            dir_entries.sort_by(|f1, f2| get_name!(f1).cmp(get_name!(f2)));

            for f in dir_entries {
                if f.file_type().unwrap().is_dir() {
                    if let Some(_t) = ui
                        .tree_node_config(get_name!(f))
                        .open_on_arrow(false)
                        .open_on_double_click(false)
                        .opened(false, imgui::Condition::Always)
                        .push()
                    {
                        self.current_dir = f.path();
                        self.picked = None;
                    }
                } else {
                    if !(self.filter)(&f.path()) && !self.show_all {
                        continue;
                    }

                    let selected = self
                        .picked
                        .as_ref()
                        .is_some_and(|picked| picked == &f.path());

                    if let Some(_t) = ui
                        .tree_node_config(
                            f.path()
                                .file_name()
                                .unwrap_or(OsStr::new(""))
                                .to_str()
                                .unwrap_or("<?>"),
                        )
                        .bullet(true)
                        .leaf(true)
                        .selected(selected)
                        .push()
                    {
                        if ui.is_item_clicked() {
                            self.picked = Some(f.path().clone());
                        }
                    }
                }
            }
        }

        selected.then_some(self.picked.clone())
    }
}

fn draw_cost_tree(
    ui: &imgui::Ui,
    tree: &mut CostTree,
    new_window: Arc<Mutex<Option<NewWindow>>>,
    root_id: u32,
) {
    macro_rules! column {
        ($str:expr, $size:expr) => {
            column!($str, $size, TableColumnFlags::empty())
        };
        ($str:expr, $size:expr, $flags:expr) => {{
            let mut t = TableColumnSetup::new($str);
            t.flags = TableColumnFlags::WIDTH_FIXED | $flags;
            t.init_width_or_weight = $size;
            let id = ui.new_id_str($str);
            t.user_id = id;
            ui.table_setup_column_with(t);
            id
        }};
    }

    if let Some(_t) = ui.begin_table_with_flags(
        "Perf",
        8,
        TableFlags::RESIZABLE
            | TableFlags::REORDERABLE
            | TableFlags::HIDEABLE
            | TableFlags::ROW_BG
            | TableFlags::SCROLL_X
            | TableFlags::SCROLL_Y
            | TableFlags::SORTABLE,
    ) {
        ui.table_setup_scroll_freeze(0, 1);
        let col_name = column!("Name", 480.0);
        let col_entries = column!("Entries", 80.0, TableColumnFlags::PREFER_SORT_DESCENDING);
        let col_inh_time = column!(
            "Inherited Time",
            120.0,
            TableColumnFlags::DEFAULT_SORT | TableColumnFlags::PREFER_SORT_DESCENDING
        );
        let col_inh_alloc = column!(
            "Inherited Alloc",
            120.0,
            TableColumnFlags::PREFER_SORT_DESCENDING
        );
        let col_indiv_time = column!(
            "Individual Time",
            120.0,
            TableColumnFlags::PREFER_SORT_DESCENDING
        );
        let col_indiv_alloc = column!(
            "Individual Alloc",
            120.0,
            TableColumnFlags::PREFER_SORT_DESCENDING
        );
        let col_module = column!("Module", 240.0);
        let col_src = column!("Source", 240.0);
        ui.table_headers_row();

        if let Some(spec) = ui.table_sort_specs_mut() {
            spec.conditional_sort(|specs| {
                if let Some(spec) = specs.iter().next() {
                    let dir = spec
                        .sort_direction()
                        .unwrap_or(TableSortDirection::Descending);

                    if to_id(spec.column_user_id()) == col_name {
                        tree.sort_by(dir, |e1, e2| e1.name.cmp(&e2.name))
                    } else if to_id(spec.column_user_id()) == col_entries {
                        tree.sort_by(dir, |e1, e2| e1.entries.value.cmp(&e2.entries.value))
                    } else if to_id(spec.column_user_id()) == col_inh_time {
                        tree.sort_by_f32(dir, |e| e.inherited_time.value)
                    } else if to_id(spec.column_user_id()) == col_inh_alloc {
                        tree.sort_by_f32(dir, |e| e.inherited_alloc.value)
                    } else if to_id(spec.column_user_id()) == col_indiv_time {
                        tree.sort_by_f32(dir, |e| e.individual_alloc.value)
                    } else if to_id(spec.column_user_id()) == col_indiv_alloc {
                        tree.sort_by_f32(dir, |e| e.individual_alloc.value)
                    } else if to_id(spec.column_user_id()) == col_module {
                        tree.sort_by(dir, |e1, e2| e1.module.cmp(&e2.module))
                    } else if to_id(spec.column_user_id()) == col_src {
                        tree.sort_by(dir, |e1, e2| e1.source.cmp(&e2.source))
                    }
                }
            });
        }

        draw_cost_subtree(&ui, tree, new_window, root_id);
    }
}

fn draw_cost_subtree(
    ui: &imgui::Ui,
    tree: &mut CostTree,
    new_window: Arc<Mutex<Option<NewWindow>>>,
    root_id: u32,
) {
    for (entry, children) in tree.entries.iter_mut() {
        ui.table_next_row();
        ui.table_next_column();

        let _id = ui.push_id_ptr(&entry.cost_center.string);

        // HACK: We want whole row to highlight on hower but that's not compatible with
        // storing `TreeNode`s in cells so we add an invisible `Selectable` on top, store open/closed
        // status in tree entry and manage that manually
        let p = ui.cursor_screen_pos();
        if ui
            .selectable_config("##nolabel")
            .flags(SelectableFlags::SPAN_ALL_COLUMNS)
            .build()
        {
            entry.opened = !entry.opened;
        }

        if let Some(_t) = ui.begin_popup_context_item() {
            if ui.menu_item("Collapse all") {
                entry.opened = false;
                children.collapse_all();
                ui.close_current_popup();
            }

            if ui.menu_item("Expand interesting") {
                if entry.is_interesting() {
                    entry.opened = true;
                    children.open_interesting();
                }
                ui.close_current_popup();
            }
            if ui.menu_item("Expand all") {
                entry.opened = true;
                children.open_all();
                ui.close_current_popup();
            }
            ui.separator();
            if ui.menu_item("Open `calls to`") {
                *new_window.lock().unwrap() = Some(NewWindow::IncomingCalls {
                    module: entry.module.to_string(),
                    name: entry.name.to_string(),
                    source: entry.source.to_string(),
                    root_id,
                });
                ui.close_current_popup();
            }
            if ui.menu_item("Open `focus on`") {
                *new_window.lock().unwrap() = Some(NewWindow::Focus {
                    name: entry.name.to_string(),
                    cost_center: entry.cost_center.value,
                    root_id,
                });
                ui.close_current_popup();
            }
            // TODO: "Open `invert at`" option
            ui.separator();
            ui.text("Click outside to close");
        }

        ui.set_cursor_screen_pos(p);
        let open = ui
            .tree_node_config(&entry.name)
            .flags(if children.entries.is_empty() {
                TreeNodeFlags::LEAF | TreeNodeFlags::BULLET
            } else {
                TreeNodeFlags::empty()
            })
            .opened(entry.opened, imgui::Condition::Always)
            .push();

        ui.table_next_column();
        ui.text(&entry.entries.string);

        let pg_height = 13.0;

        ui.table_next_column();
        ProgressBar::new(entry.inherited_time.value / 100.0)
            .size([-1.0, pg_height])
            .build(ui);

        ui.table_next_column();
        ProgressBar::new(entry.inherited_alloc.value / 100.0)
            .size([-1.0, pg_height])
            .build(ui);

        ui.table_next_column();
        ProgressBar::new(entry.individual_time.value / 100.0)
            .size([-1.0, pg_height])
            .build(ui);

        ui.table_next_column();
        ProgressBar::new(entry.individual_alloc.value / 100.0)
            .size([-1.0, pg_height])
            .build(ui);

        // FIXME: This text is a bit too low?
        ui.table_next_column();
        ui.text(&entry.module);

        ui.table_next_column();
        ui.text(&entry.source);

        if let Some(_open) = open {
            draw_cost_subtree(ui, children, new_window.clone(), root_id);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Displayed<T> {
    value: T,
    string: String,
}

impl<T> Displayed<T>
where
    T: Display,
{
    pub fn new(value: T) -> Displayed<T> {
        Displayed {
            string: value.to_string(),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct Entry {
    cost_center: Displayed<u64>,
    name: String,
    entries: Displayed<u64>,
    individual_time: Displayed<f32>,
    individual_alloc: Displayed<f32>,
    inherited_time: Displayed<f32>,
    inherited_alloc: Displayed<f32>,
    module: String,
    source: String,
    opened: bool,
}

impl Entry {
    fn is_interesting(&self) -> bool {
        self.inherited_time.value >= 15.0
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct CostTree {
    entries: Vec<(Entry, CostTree)>,
}

impl CostTree {
    fn sort_by_f32(&mut self, dir: TableSortDirection, f: impl Copy + Fn(&Entry) -> f32) {
        match dir {
            TableSortDirection::Ascending => {
                self.entries.sort_unstable_by(|(entry1, _), (entry2, _)| {
                    f32::total_cmp(&f(entry1), &f(entry2))
                })
            }
            TableSortDirection::Descending => {
                self.entries.sort_unstable_by(|(entry1, _), (entry2, _)| {
                    f32::total_cmp(&f(entry2), &f(entry1))
                })
            }
        }

        self.entries
            .iter_mut()
            .for_each(|(_, children)| children.sort_by_f32(dir, f));
    }

    fn sort_by(&mut self, dir: TableSortDirection, f: impl Copy + Fn(&Entry, &Entry) -> Ordering) {
        match dir {
            TableSortDirection::Ascending => self
                .entries
                .sort_unstable_by(|(entry1, _), (entry2, _)| f(entry2, entry1)),

            TableSortDirection::Descending => self
                .entries
                .sort_unstable_by(|(entry1, _), (entry2, _)| f(entry1, entry2)),
        }

        self.entries
            .iter_mut()
            .for_each(|(_, children)| children.sort_by(dir, f));
    }

    fn open_interesting(&mut self) {
        for (entry, children) in self.entries.iter_mut() {
            if entry.is_interesting() {
                entry.opened = true;
                children.open_interesting();
            }
        }
    }

    fn open_all(&mut self) {
        for (entry, children) in self.entries.iter_mut() {
            entry.opened = true;
            children.open_all();
        }
    }

    fn collapse_all(&mut self) {
        for (entry, children) in self.entries.iter_mut() {
            entry.opened = false;
            children.collapse_all();
        }
    }

    fn find<'a>(&'a self, target_module: &str, target_source: &str, target_name: &str) -> CostTree {
        let mut entries = Vec::new();

        for (entry, child_tree) in &self.entries {
            let filtered_tree = child_tree.find(target_module, target_source, target_name);

            if (entry.module == target_module
                && entry.source == target_source
                && entry.name == target_name)
                || !filtered_tree.entries.is_empty()
            {
                entries.push((entry.clone(), filtered_tree));
            }
        }

        entries.iter_mut().for_each(|(entry, children)| {
            if entry.module == target_module
                && entry.source == target_source
                && entry.name == target_name
            {
                return;
            }

            let mut inherited_time = 0.0;
            let mut inherited_alloc = 0.0;
            for (child, _) in &children.entries {
                inherited_time += child.inherited_time.value;
                inherited_alloc += child.inherited_alloc.value;
            }
            entry.inherited_time = Displayed::new(inherited_time);
            entry.inherited_alloc = Displayed::new(inherited_alloc);
        });
        CostTree { entries }
    }

    fn focus(&self, cost_center: u64) -> Option<CostTree> {
        for (entry, children) in &self.entries {
            if entry.cost_center.value == cost_center {
                return Some(CostTree {
                    entries: vec![(entry.clone(), children.clone())],
                });
            }
            if let Some(needle) = children.focus(cost_center) {
                return Some(needle.clone());
            }
        }
        None
    }
}

#[derive(Debug)]
enum ParserError {
    EndOfInput,
    ParseFloatError(ParseFloatError, String),
    ParseIntError(ParseIntError, String),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EndOfInput => write!(f, "Unexpected end of input"),
            ParserError::ParseFloatError(err, line) => {
                write!(f, "Could not parse float in line `{line}`: {err}")
            }
            ParserError::ParseIntError(err, line) => {
                write!(f, "Could not parse integer in line `{line}`: {err}")
            }
        }
    }
}

const NO_LOCATION_INFO: &'static str = "<no location info>";

struct Parser<'src> {
    lines: Lines<'src>,
    line: Option<&'src str>,
}

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Result<Self, ParserError> {
        let mut s = Self {
            lines: input.lines(),
            line: None,
        };
        s.go_to_start()?;
        Ok(s)
    }

    fn go_to_start(&mut self) -> Result<(), ParserError> {
        while !self
            .lines
            .next()
            .ok_or_else(|| ParserError::EndOfInput)?
            .starts_with("COST CENTRE")
        {}
        while !self
            .lines
            .next()
            .ok_or_else(|| ParserError::EndOfInput)?
            .starts_with("COST CENTRE")
        {}
        self.lines.next().ok_or_else(|| ParserError::EndOfInput)?;
        self.line = self.lines.next();
        Ok(())
    }

    pub fn next_indent(&self) -> Option<u32> {
        self.line
            .map(|line| line.chars().take_while(|&c| c == ' ').count() as u32 + 1)
    }

    pub fn parse_entry(&mut self) -> Result<Option<Entry>, ParserError> {
        let Some(line) = self.line else {
            return Ok(None);
        };

        let mut values = line.split_ascii_whitespace();

        macro_rules! parse_value {
            (str) => {{
                let start = values.next().ok_or_else(|| ParserError::EndOfInput)?;
                if start == "<no" {
                    // HACK: We split by whitespace for simplicity of parsing but
                    // `<no location info>` location is special and has spaces.
                    // We assume that this is the only case and hard code it here
                    values.next().ok_or_else(|| ParserError::EndOfInput)?;
                    values.next().ok_or_else(|| ParserError::EndOfInput)?;
                    NO_LOCATION_INFO.to_owned()
                } else {
                    start.to_owned()
                }
            }};
            (u64) => {{
                let string = values.next().ok_or_else(|| ParserError::EndOfInput)?;
                let parsed = string
                    .parse::<u64>()
                    .map_err(|err| ParserError::ParseIntError(err, line.to_string()))?;
                Displayed::new(parsed)
            }};
            (f32) => {{
                let string = values.next().ok_or_else(|| ParserError::EndOfInput)?;
                let parsed = string
                    .parse::<f32>()
                    .map_err(|err| ParserError::ParseFloatError(err, line.to_string()))?;
                Displayed::new(parsed)
            }};
        }

        let entry = Entry {
            name: parse_value!(str),
            module: parse_value!(str),
            source: parse_value!(str),
            cost_center: parse_value!(u64),
            entries: parse_value!(u64),
            individual_time: parse_value!(f32),
            individual_alloc: parse_value!(f32),
            inherited_time: parse_value!(f32),
            inherited_alloc: parse_value!(f32),
            opened: false,
        };

        self.line = self.lines.next();

        Ok(Some(entry))
    }

    fn parse_subtree(&mut self, parent_indent: u32) -> Result<CostTree, ParserError> {
        let mut entries = Vec::new();

        while let Some(next_indent) = self.next_indent() {
            if next_indent <= parent_indent {
                break;
            }

            let entry = self
                .parse_entry()?
                .expect("unreachable: pattern match above");
            let subtree = self.parse_subtree(next_indent)?;
            entries.push((entry, subtree));
        }

        Ok(CostTree { entries })
    }

    fn parse_tree(&mut self) -> Result<CostTree, ParserError> {
        let mut t = self.parse_subtree(0)?;
        for (entry, _) in t.entries.iter_mut() {
            entry.opened = true;
        }
        Ok(t)
    }
}

#[derive(Debug)]
enum NewWindow {
    IncomingCalls {
        module: String,
        source: String,
        name: String,
        root_id: u32,
    },
    Focus {
        name: String,
        cost_center: u64,
        root_id: u32,
    },
    Profile {
        input_file: PathBuf,
    },
}

struct Window {
    title: String,
    tree: CostTree,
    root_id: u32,
    fname: String,
}

fn to_id(id: u32) -> Id {
    // No safe wrapper
    unsafe { core::mem::transmute(id) }
}
