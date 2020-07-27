// Expands upon Avalonia.FuncUI.DSL
namespace Nir.DSL

[<AutoOpen>]
module DragDrop =
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.Interactivity
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types

    // let create (attrs: IAttr<DragDrop> list): IView<DragDrop> = ViewBuilder.Create<DragDrop>(attrs)

    // type Control with
    //     static member allowDrop<'t when 't :> Control> (allow: bool): IAttr<'t> =
    //         AttrBuilder<'t>.CreateProperty<bool> (DragDrop.AllowDropProperty, allow, ValueNone)

    type DragDrop with
    
        // fsharplint:disable MemberNames TupleCommaSpacing
        // The question marks confuse TupleCommaSpacing
        static member onDragEnter<'t when 't :> Control> (func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DragEnterEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDragLeave<'t when 't :> Control> (func: RoutedEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<RoutedEventArgs>
                (DragDrop.DragLeaveEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDragOver<'t when 't :> Control> (func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DragOverEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDrop<'t when 't :> Control> (func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DropEvent, func, ?subPatchOptions = subPatchOptions)

        static member allowDrop<'t when 't :> Control> (allow: bool): IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<bool> (DragDrop.AllowDropProperty, allow, ValueNone)
