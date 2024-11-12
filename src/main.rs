use imgui::{
    Id, ProgressBar, SelectableFlags, TableColumnFlags, TableColumnSetup, TableFlags,
    TableSortDirection, TreeNodeFlags,
};
use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt::Display,
    fs::File,
    io::Read,
    num::{ParseFloatError, ParseIntError},
    str::Lines,
    sync::{Arc, Mutex},
};

mod boilerplate_sdl2;

fn main() {
    macro_rules! err_exit {
        () => {
            err_exit!("imghcprof: usage: imghcprof <input.prof>")
        };
        ($($arg:tt)*) => {{
            eprintln!($($arg)*);
            std::process::exit(1)
        }};
    }

    let mut args = std::env::args().into_iter();
    let _prog = args.next().unwrap_or_else(|| err_exit!());
    let input_file = args.next().unwrap_or_else(|| err_exit!());
    if !args.next().is_none() {
        err_exit!();
    }

    let mut input = String::new();
    let mut f = File::open(&input_file).unwrap_or_else(|err| {
        err_exit!("imghcprof: Could not open input file '{input_file}': {err}")
    });
    f.read_to_string(&mut input).unwrap_or_else(|err| {
        err_exit!("imghcprof: Could not read input file '{input_file}': {err}")
    });

    let mut parser = Parser::new(&input).unwrap_or_else(|err| {
        err_exit!("imghcprof: Could not parse input file '{input_file}': {err}")
    });
    let mut tree = parser.parse_tree().unwrap_or_else(|err| {
        err_exit!("imghcprof: Could not parse input file '{input_file}': {err}")
    });
    tree.open_interesting();

    let mut first_run = true;
    let mut windows = Vec::new();
    windows.push((
        None,
        Window {
            title: "Profile".to_string(),
            tree,
        },
    ));

    // Windows request new windows via this mutex
    let new_window: Arc<Mutex<Option<NewWindow>>> = Arc::new(Mutex::new(None));

    // There can be duplicate window titles so we add invisible counter
    let mut next_window_no = 0;

    boilerplate_sdl2::draw("imghcprof", |ui| {
        // Create main tab with profiler window
        unsafe {
            let dockspace = imgui_sys::igDockSpaceOverViewport(
                imgui_sys::igGetMainViewport(),
                imgui_sys::ImGuiDockNodeFlags_PassthruCentralNode as i32,
                core::ptr::null(),
            );
            if first_run {
                first_run = false;
                imgui_sys::igDockBuilderDockWindow(c"Profile".as_ptr().cast(), dockspace);
            }
            dockspace
        };

        // TODO: File selector
        // if let Some(_main_menu) = ui.begin_main_menu_bar() {
        //     if let Some(_new_menu) = ui.begin_menu("Open") {}
        // }

        for (is_opened, window) in &mut windows {
            let mut w = ui
                .window(&window.title)
                .menu_bar(false)
                .collapsible(false)
                .title_bar(true)
                .resizable(true);

            if let Some(is_opened) = is_opened {
                w = w.opened(is_opened);
            }

            w.build(|| {
                draw_cost_tree(&ui, &mut window.tree, new_window.clone());
            });
        }

        windows.retain(|(is_opened, _)| match is_opened {
            None => true,
            Some(true) => true,
            Some(false) => false,
        });

        if let Some(new) = new_window.lock().unwrap().take() {
            match new {
                NewWindow::IncomingCalls {
                    module,
                    source,
                    name,
                } => {
                    let new_tree = CostTree::find(&windows[0].1.tree, &module, &source, &name);
                    let title = format!("Calls to `{name}` ## {next_window_no}");
                    windows.push((
                        Some(true),
                        Window {
                            title,
                            tree: new_tree,
                        },
                    ));
                    next_window_no += 1;
                }
                NewWindow::Focus { name, cost_center } => {
                    if let Some(new_tree) = CostTree::focus(&windows[0].1.tree, cost_center) {
                        let title = format!("Focus on `{name}` ## {next_window_no}");
                        windows.push((
                            Some(true),
                            Window {
                                title,
                                tree: new_tree,
                            },
                        ));
                        next_window_no += 1;
                    }
                }
            }
        }
    });
}

fn draw_cost_tree(ui: &imgui::Ui, tree: &mut CostTree, new_window: Arc<Mutex<Option<NewWindow>>>) {
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

        draw_cost_subtree(&ui, tree, new_window);
    }
}

fn draw_cost_subtree(
    ui: &imgui::Ui,
    tree: &mut CostTree,
    new_window: Arc<Mutex<Option<NewWindow>>>,
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
                });
                ui.close_current_popup();
            }
            if ui.menu_item("Open `focus on`") {
                *new_window.lock().unwrap() = Some(NewWindow::Focus {
                    name: entry.name.to_string(),
                    cost_center: entry.cost_center.value,
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
            draw_cost_subtree(ui, children, new_window.clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Displayed<'src, T> {
    value: T,
    string: Cow<'src, str>,
}

impl<'src, T> Displayed<'src, T>
where
    T: Display,
{
    pub fn borrowed(value: T, string: &'src str) -> Displayed<T> {
        Displayed {
            value,
            string: Cow::Borrowed(string),
        }
    }

    pub fn owned(value: T) -> Displayed<'static, T> {
        Displayed {
            string: Cow::Owned(value.to_string()),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct Entry<'src> {
    cost_center: Displayed<'src, u64>,
    name: Cow<'src, str>,
    entries: Displayed<'src, u64>,
    individual_time: Displayed<'src, f32>,
    individual_alloc: Displayed<'src, f32>,
    inherited_time: Displayed<'src, f32>,
    inherited_alloc: Displayed<'src, f32>,
    module: Cow<'src, str>,
    source: Cow<'src, str>,
    opened: bool,
}

impl<'src> Entry<'src> {
    fn is_interesting(&self) -> bool {
        self.inherited_time.value >= 95.0
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct CostTree<'src> {
    entries: Vec<(Entry<'src>, CostTree<'src>)>,
}

impl<'src> CostTree<'src> {
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

    fn find<'a>(
        &'a self,
        target_module: &str,
        target_source: &str,
        target_name: &str,
    ) -> CostTree<'src> {
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
            entry.inherited_time = Displayed::owned(inherited_time);
            entry.inherited_alloc = Displayed::owned(inherited_alloc);
        });
        CostTree { entries }
    }

    fn focus(&self, cost_center: u64) -> Option<CostTree<'src>> {
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

    pub fn parse_entry(&mut self) -> Result<Option<Entry<'src>>, ParserError> {
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
                    Cow::Borrowed(NO_LOCATION_INFO)
                } else {
                    Cow::Borrowed(start)
                }
            }};
            (u64) => {{
                let string = values.next().ok_or_else(|| ParserError::EndOfInput)?;
                let parsed = string
                    .parse::<u64>()
                    .map_err(|err| ParserError::ParseIntError(err, line.to_string()))?;
                Displayed::borrowed(parsed, string)
            }};
            (f32) => {{
                let string = values.next().ok_or_else(|| ParserError::EndOfInput)?;
                let parsed = string
                    .parse::<f32>()
                    .map_err(|err| ParserError::ParseFloatError(err, line.to_string()))?;
                Displayed::borrowed(parsed, string)
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

    fn parse_subtree(&mut self, parent_indent: u32) -> Result<CostTree<'src>, ParserError> {
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

    fn parse_tree(&mut self) -> Result<CostTree<'src>, ParserError> {
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
    },
    Focus {
        name: String,
        cost_center: u64,
    },
}

struct Window<'src> {
    title: String,
    tree: CostTree<'src>,
}

fn to_id(id: u32) -> Id {
    // No safe wrapper
    unsafe { core::mem::transmute(id) }
}
