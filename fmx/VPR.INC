(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Vectorial Polygon Rasterizer for FMX.
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{ FONTS }

// disable font hinting
{.$DEFINE NOHINTING}

// enable cleartype text rendering
{$DEFINE CLEARTYPE}

// enable high quality cleartype filtering
{$DEFINE CLEARTYPEHQ}

// enable cleartype with a different filtering scheme
{.$DEFINE CLEARTYPESMOOTH}

// disable horizontal hinting by stretching text horizontally
// source: Maxim Shemanarev, "Texts Rasterization Exposures",
// <http://www.antigrain.com/research/font_rasterization/index.html>
{$DEFINE NOHORIZONTALHINTING}


{ GRADIENTS }

// use look-up table for gradients (faster)
{$DEFINE GRADIENTLUT}


{ GENERAL }

// enable gamma correction
{$DEFINE GAMMACORRECTION}

// do not set the DefaultCanvas variable upon initialization
{.$DEFINE DISABLEAUTOINITIALIZE}

// use only pure pascal routines
{.$DEFINE PUREPASCAL}

// use stack memory allocation routine (32-bit only)
{$DEFINE USESTACKALLOC}

{$IFDEF USESTACKALLOC}
{-$W+}
{$ENDIF}

// disable conversions to double for single-precision expressions (major speed-up)
{$EXCESSPRECISION OFF}
