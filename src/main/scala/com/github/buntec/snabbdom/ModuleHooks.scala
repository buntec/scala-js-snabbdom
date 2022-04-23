package com.github.buntec.snabbdom

final case class ModuleHooks(
    create: List[CreateHook],
    update: List[UpdateHook],
    remove: List[RemoveHook],
    destroy: List[DestroyHook],
    pre: List[PreHook],
    post: List[PostHook]
)

object ModuleHooks {

  def empty = ModuleHooks(Nil, Nil, Nil, Nil, Nil, Nil)

}
