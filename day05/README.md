# Day 5

On our journey through the programming languages of the 60s, we can of course not skip the [B programming language][1], one of the more influential predecessors of C. Both B and C, being developed at Bell Labs by Ken Thompson and Dennis Ritchie, exhibit large similarities in their syntax, yet B ended up [being quickly succeeded by C][2] and was therefore never used widely. This made it quite challenging to find a B compiler that runs on modern systems. The few systems that still use B [target mainframes][3] rather than mainstream x86_64/ARM hardware.

Fortunately, a few hobbyists have taken on the challenge of building a B compiler based on the Ken Thompson book, in particular C. Smith, [whose implementation][4] I will use for this challenge. To quote the website:

> It is implemented by compiling to machine code for a fictitious machine (actually a family of machines) which is then run by an instruction set simulator. The reason I mention a family of machines is because although it primarily supports the version of B described by Ken Thompson in his original reference manual for B on the 16-bit PDP-11 minicomputer (which I have followed as rigorously as possible)

With a [little bit of patching](b-compiler/patches), most notably pinning down the C++ standard to C++03, this compiler runs more or less out-of-the-box on modern POSIX systems such as macOS or Linux. By targeting bytecode rather than native code the compiler also sidesteps the portability issues with assembly or machine code generation.

[1]: https://en.wikipedia.org/wiki/B_(programming_language)
[2]: https://web.archive.org/web/20150611114404/https://www.bell-labs.com/usr/dmr/www/bintro.html
[3]: https://stackoverflow.com/a/1601432/19890279
[4]: https://cpjsmith.uk/b
