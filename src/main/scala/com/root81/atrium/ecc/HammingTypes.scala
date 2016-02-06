//
// HammingTypes.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.ecc

case class InvalidLengthException(message: String) extends Exception(message)
case class ByteCorruptionException(distance: Int, message: String) extends Exception(message)

