package com.github.buntec.snabbdom

object thunk {

  def apply(
      sel: String,
      fn: Seq[Any] => VNode,
      args: Seq[Any]
  ): VNode = {
    val data = VNodeData.empty
    data.fn = Some(fn)
    data.args = Some(args)
    h(sel, data)
  }

  def apply(
      sel: String,
      key: String,
      fn: Seq[Any] => VNode,
      args: Seq[Any]
  ): VNode = {
    val data = VNodeData.empty
    data.key = Some(key)
    data.fn = Some(fn)
    data.args = Some(args)
    data.hook = Some(
      new Hooks {
        override def init: Option[InitHook] =
          Some((vNode: VNode) => init0(vNode))
        override def prepatch: Option[PrePatchHook] =
          Some((oldVNode: VNode, vNode: VNode) => prepatch0(oldVNode, vNode))
      }
    )
    h(sel, data)
  }

  private def init0(thunk: VNode): Unit = {
    val data = thunk.data.get
    val fn = data.fn.get
    val args = data.args.get
    copyToThunk(fn(args), thunk)
  }

  private def prepatch0(oldVnode: VNode, thunk: VNode): Unit = {
    val old = oldVnode.data
    val cur = thunk.data
    val oldArgs = old.flatMap(_.args)
    val args = cur.flatMap(_.args)
    val oldFn = old.flatMap(_.fn)
    val curFn = cur.flatMap(_.fn)
    if (oldFn != curFn || oldArgs != args) {
      copyToThunk(curFn.get(args.get), thunk)
    } else {
      copyToThunk(oldVnode, thunk)
    }
  }

  private def copyToThunk(vnode: VNode, thunk: VNode): Unit = {
    val ns = thunk.data.flatMap(_.ns)
    vnode.data.foreach(_.fn = thunk.data.flatMap(_.fn))
    vnode.data.foreach(_.args = thunk.data.flatMap(_.args))
    thunk.data = vnode.data
    thunk.children = vnode.children
    thunk.text = vnode.text
    thunk.elm = vnode.elm
    ns.foreach(_ => h.addNS(thunk.data.get, thunk.children, thunk.sel))
  }

}
