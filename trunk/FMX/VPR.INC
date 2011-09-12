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

// do not set the DefaultCanvas variable upon initialization
{.$DEFINE DISABLEAUTOINITIALIZE}

// enable cleartype text rendering
{$DEFINE CLEARTYPE}

// enable high quality cleartype filtering
{$DEFINE CLEARTYPEHQ}

// enable cleartype with a different filtering scheme
{.$DEFINE CLEARTYPESMOOTH}

// enable gamma correction
{$DEFINE GAMMACORRECTION}

// use look-up table for gradients (faster)
{$DEFINE GRADIENTLUT}

// disable font hinting
{.$DEFINE NOHINTING}

// use stack memory allocation routine (32-bit only)
{$DEFINE USESTACKALLOC}

// use only pure pascal routines
{.$DEFINE PUREPASCAL}

// disable conversions to double for single-precision expressions (major speed-up)
{$EXCESSPRECISION OFF}
