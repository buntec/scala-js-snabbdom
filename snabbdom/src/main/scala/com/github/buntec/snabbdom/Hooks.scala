package com.github.buntec.snabbdom

trait Hooks {

  def pre: Option[PreHook] = None

  def init: Option[InitHook] = None

  def create: Option[CreateHook] = None

  def insert: Option[InsertHook] = None

  def prepatch: Option[PrePatchHook] = None

  def update: Option[UpdateHook] = None

  def postpatch: Option[PostPatchHook] = None

  def destroy: Option[DestroyHook] = None

  def remove: Option[RemoveHook] = None

  def post: Option[PostHook] = None

}
