package com.github.buntec.snabbdom

final case class Module(
    pre: Option[PreHook] = None,
    create: Option[CreateHook] = None,
    update: Option[UpdateHook] = None,
    destroy: Option[DestroyHook] = None,
    remove: Option[RemoveHook] = None,
    post: Option[PostHook] = None
)
