package com.github.buntec.snabbdom

final case class Module(
    pre: Option[PreHook],
    create: Option[CreateHook],
    update: Option[UpdateHook],
    destroy: Option[DestroyHook],
    remove: Option[RemoveHook],
    post: Option[PostHook]
)
