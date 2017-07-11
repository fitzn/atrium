Atrium -- Steganography with JPEGs
=====================================

Embed text messages into JPEGs without altering their visual perception.

Atrium is a proof-of-concept command line application that
enables you to embed text into a JPEG image without perceptibly altering it.

This is a toy. It's probably not useful in your real-world use case.
See the [Discussion][disc] section for more information.


Background and Motivation
----------

[Steganography][stego] is the technique of concealing information
within some other artifact that is ostensibly only carrying
the immediately or visually apparent information.
Any medium can be used as the "host" to carry the secret message.

The [JPEG][jpeg] image format is ubiquitous but
potentially a poor steganographic carrier because it is a lossy file format.
This means that [modulated][mod] RGB pixel values in an image
might be altered during the JPEG image encoding (i.e., compression),
rendering the secret message encoded by the modulation unreadable.


What is Atrium?
------------

Atrium is a proof-of-concept application for embedding information
in an image's perceived visual content, rather than its RGB pixels.

Atrium embeds a secret text message into a JPEG
by modulating the quantized values of the underlying [DCT][dct] coefficients
to achieve parity with bits in the message.
This makes Atrium resistent to JPEG's lossy format.

Additionally, human eyes cannot detect small visual changes -
in fact, that is what JPEG is exploiting in the first place -
so, the visual changes due to Atrium's encoding are often imperceptible.
The file size overhead is minimal (~3KB for a small message) as well.

Atrium embeds one byte of text into each 8x8 pixel region,
but it still relies on Java itself to write out the encoded JPEG.
This ensures the output image is a valid JPEG image no matter what.


Installation
-----

1. Download and install a [JVM][jvm].
2. Download and install [Scala][scala].
3. Download the Atrium Project.
4. Compile the project:

```bash
$ cd atrium
$ mvn clean install
```


Usage
-----

The basic usage is to **encode** text into an image, and then **decode** it.
The application also offers an **info** command,
which prints descriptive information about a JPEG image.
Run `atrium` by itself to see help information.


#### Encode

To encode text into an image:

```bash
$ ./atrium encode --out encoded.jpg image.jpg "Hello, atrium."
```

| Input Image | Encoded Image |
| ----------- | ------------- |
| ![][input]  | ![][encoded]  |

The output image defaults to the JPEG compression quality of the input image,
but you can configure the encoded output image's quality with the
`--quality` argument.

If an output file path is not specified,
atrium defaults to the input file name with "-atrium" appended:

```bash
$ ./atrium encode image.jpg "Hello, atrium."
$ du -h image*.jpg
  124K  image-atrium.jpg
  120K  image.jpg
```


#### Decode

To decode the text from an Atrium-encoded image:

```bash
$ ./atrium decode encoded.jpg
  Hello, atrium.
```


#### Info

To print file information for a JPG image:

```bash
$ ./atrium info image.jpg
```


Discussion
------

As mentioned already, this project is purely a proof-of-concept.
There are a number of limitations including:

1. Atrium is untested for long input messages.

2. Atrium doesn't work for pure white backgrounds.
  This is because Luminance values won't decode if wrapped-around at 248,
  so Atrium caps them at 248, which chops off the DCT modulation.
  See the [ball][ball] image.

3. High and low JPEG compression qualities are untested (i.e., unsupported).
  Testing showed Atrium to be most effective in the 60 - 80 JPEG quality range.
  You'll notice that the [roman-atrium][roman] image fails
  to decode a long message with default encoding due to its high (99) quality.
  If you encode with a lower quality (e.g., 80), decoding works.

Still, even with its limitations, Atrium works well under the right conditions.
For example, Atrium encoded a message into this [Instagram post][post] and
successfully decodes the message from the downloaded image.
This is despite Instagram's re-encoding and metadata stripping process.


Conclusion
----

Thanks for checking it out.


[disc]: https://github.com/fitzn/atrium#discussion
[stego]: https://en.wikipedia.org/wiki/Steganography
[jpeg]: https://en.wikipedia.org/wiki/JPEG
[mod]: https://en.wikipedia.org/wiki/Modulation
[jvm]: https://www.java.com/en/download/
[scala]: http://www.scala-lang.org/download/
[dct]: http://www.programming-techniques.com/2014/02/discrete-cosine-transform-and-jpeg.html
[input]: https://github.com/fitzn/atrium/blob/master/src/test/resources/images/shoes.jpg
[encoded]: https://github.com/fitzn/atrium/blob/master/src/test/resources/images/shoes-atrium.jpg
[roman]: https://github.com/fitzn/atrium/blob/master/src/test/resources/images/roman-atrium.jpg
[ball]: https://github.com/fitzn/atrium/blob/master/src/test/resources/images/ball.jpg
[post]: https://www.instagram.com/p/BWY0LHhAbkx/
