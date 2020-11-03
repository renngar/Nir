namespace Nir.UI

/// How the page is treated by the history system as manifest through the back and forward buttons.
type HistoryStyle =
    /// Participate normally in the history system
    | Normal
    /// Does not get added to the history.
    ///
    /// Preserves the state behind the forward button, but disables it while active.
    | NoHistory
    /// Does not participate in history and disables navigation controls and menus.
    | Modal

type IPageModel =
    inherit IPluginModel
    abstract HistoryStyle: HistoryStyle
