// Drag-and-drop support for Avalonia.FuncUI.
//
// Copyright (C) 2020 Renngar <renngar@renngar.com>
//
// This file is part of Nir.
//
// Nir is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.

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
        static member onDragEnter<'t when 't :> Control>(func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DragEnterEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDragLeave<'t when 't :> Control>(func: RoutedEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<RoutedEventArgs>
                (DragDrop.DragLeaveEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDragOver<'t when 't :> Control>(func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DragOverEvent, func, ?subPatchOptions = subPatchOptions)

        static member onDrop<'t when 't :> Control>(func: DragEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragEventArgs>
                (DragDrop.DropEvent, func, ?subPatchOptions = subPatchOptions)

        static member allowDrop<'t when 't :> Control>(allow: bool): IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<bool>(DragDrop.AllowDropProperty, allow, ValueNone)
