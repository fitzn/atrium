//
// AtriumLogger.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.text.SimpleDateFormat
import java.util.Date

object AtriumLogger {

  val DEBUG = Option(System.getProperties.get("com.root81.atrium.debug")).map(_.toString).contains("true")

  private val DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSSZ"
  private val dateFormatter = new SimpleDateFormat(DATE_FORMAT)

  def debug(message: String): Unit = {
    if (DEBUG) {
      log("DEBUG: " + message)
    }
  }

  def info(message: String): Unit = {
    log("INFO: " + message)
  }

  def error(message: String): Unit = {
    log("ERROR: " + message)
  }

  def warn(message: String): Unit = {
    log("WARN: " + message)
  }

  //
  // Internal helpers
  //

  private def log(str: String): Unit = {
    val prefix = dateFormatter.format(new Date())
    println(s"$prefix - $str")
  }
}
