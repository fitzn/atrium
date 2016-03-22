//
// AtriumLogger.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.text.SimpleDateFormat
import java.util.Date

object AtriumLogger {

  val DEBUG = Option(System.getProperties.get("com.root81.atrium.debug")).map(_.toString.trim.toLowerCase).contains("true")

  private val DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSSZ"
  private val dateFormatter = new SimpleDateFormat(DATE_FORMAT)

  private val DEBUG_LEVEL = "DEBUG"
  private val ERROR_LEVEL = "ERROR"
  private val INFO_LEVEL = "INFO"
  private val WARN_LEVEL = "WARN"

  def debug(message: String): Unit = {
    if (DEBUG) {
      log(DEBUG_LEVEL, message)
    }
  }

  def info(message: String): Unit = {
    log(INFO_LEVEL, message)
  }

  def error(message: String): Unit = {
    log(ERROR_LEVEL, message)
  }

  def warn(message: String): Unit = {
    log(WARN_LEVEL, message)
  }

  //
  // Internal helpers
  //

  private def log(level: String, str: String): Unit = {
    val prefix = dateFormatter.format(new Date())
    println(s"$prefix - $level: $str")
  }
}
