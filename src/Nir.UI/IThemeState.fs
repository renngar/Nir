namespace Nir.UI

type IThemeState =
    abstract IsLight: bool
    abstract Toggle: unit -> unit
