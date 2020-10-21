namespace Nir.UI

type IThemeSwitcher =
    /// Is the light theme active?
    ///
    /// This may be used by UI components to display a selector.
    abstract IsLight: bool

    /// Toggles between styles.
    abstract Toggle: unit -> unit

    /// Loads plugin styles.
    ///
    /// Should be called when a plugin is made active.
    abstract LoadPluginStyles: IPlugin -> unit
