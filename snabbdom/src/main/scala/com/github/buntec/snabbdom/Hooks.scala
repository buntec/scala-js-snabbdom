package com.github.buntec.snabbdom

trait Hooks {

  def pre: Option[PreHook]

  def init: Option[InitHook]

  def create: Option[CreateHook]

  def insert: Option[InsertHook]

  def prepatch: Option[PrePatchHook]

  def update: Option[UpdateHook]

  def postpatch: Option[PostPatchHook]

  def destroy: Option[DestroyHook]

  def remove: Option[RemoveHook]

  def post: Option[PostHook]

}
