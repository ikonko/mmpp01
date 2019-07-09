# mmpp01
Manic Miner for PP-01 (SinDiKat version)


This code is derived from https://skoolkid.gitlab.io/manicminer/ and the project goal is to convert it to Czechoslovakian computer of 1980's called ZVT PP-01. Basic specs of this computer: Intel 8080 derivate (Tesla MHB 8080) at 2MHz CPU clock, 64kB of RAM, 16k of ROM with GBASIC, graphics is 256x256 pixels in 8 colors - every pixel has its own color.

Similar project exists in a binary form and supports only B/W graphics. This was coded by OldComp.cz user Stamil and is available https://pp01.borik.net/index.php?pg=manic

Current version of this particular port (from SinDiKat) is not finished yet, but it's ultimate goal is to make the game more playable, enjoyable and colorful. The porting activities started before Stamil released his version. Due to existence of his port I paused my activities on the version published here.

The code uses Z80 assembly mnemonics as per original disassembly project of Manic Miner and it's compilable by Pasmo assembler with `-8` flag to enable i8080 compatibility warnings.
