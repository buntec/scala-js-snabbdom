package com.github.buntec.snabbdom

final case class Hooks(
    pre: Option[PreHook] = None,
    init: Option[InitHook] = None,
    create: Option[CreateHook] = None,
    insert: Option[InsertHook] = None,
    prepatch: Option[PrePatchHook] = None,
    update: Option[UpdateHook] = None,
    postpatch: Option[PostPatchHook] = None,
    destroy: Option[DestroyHook] = None,
    remove: Option[RemoveHook] = None,
    post: Option[PostHook] = None
)
