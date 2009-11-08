{******************************************************************************}
{*                                                                            *}
{*  LibArt_LGPL for Windows                                                   *}
{*  <http://gnuwin32.sourceforge.net/packages/libart_lgpl.htm>                *}
{*                                                                            *}
{*  Translator(s):                                                            *}
{*    Mattias Andersson <mattias@centaurix.com>                               *}
{*                                                                            *}
{*  Last update: 2009-02-26                                                   *}
{*                                                                            *}
{******************************************************************************}

unit libart;

interface

type
  PArtPoint = ^TArtPoint;
  TArtPoint = record
    x, y: Double;
  end;

  PArtDRect = ^TArtDRect;
  TArtDRect = record
    x0, y0, x1, y1: Double;
  end;

  PArtIRect = ^TArtIRect;
  TArtIRect = record
    x0, y0, x1, y1: Integer;
  end;

  TArtPathCode = (
    ART_MOVETO,
    ART_MOVETO_OPEN,
    ART_CURVETO,
    ART_LINETO,
    ART_END
  );

  { CURVETO is not allowed! }
  PArtVpath = ^TArtVpath;
  TArtVpath = record
    Code: TArtPathcode;
    X: Double;
    Y: Double;
  end;

  TArrayOfVPath = array of TArtVPath;

  TArtSVPSeg  = record
    n_points: Integer;
    dir: Integer; { == 0 for "up", 1 for "down" }
    bbox: TArtDRect;
    points: PArtPoint;
  end;

  PArtSVP = ^TArtSVP;
  TArtSVP = record
    n_segs: Integer;
    segs: array [0..0] of TArtSVPSeg; // segs[1];
  end;

  PArtAlphaGamma = ^TArtAlphaGamma;
  TArtAlphaGamma = record
    gamma: Double;
    invtable_size: Integer;
    table: array [Byte] of Integer;
    invtable: array[0..0] of Byte;
  end;

  PArtSVPRenderAAStep = ^TArtSVPRenderAAStep;
  TArtSVPRenderAAStep = record
    x: Integer;
    delta: Integer; { stored with 16 fractional bits }
  end;

  TArtSVPCallback = procedure(callback_data: Pointer; y, start: Integer; steps: PArtSVPRenderAAStep; n_steps: Integer); cdecl;


{ Function prototypes }

procedure art_svp_free(svp: PArtSVP); cdecl; external 'libart_lgpl_22.dll';
function art_svp_from_vpath(vpath: PArtVpath): PArtSVP; cdecl; external 'libart_lgpl_22.dll';
procedure art_rgb_fill_run(buf: PByte; r, g, b: Byte; n: Integer); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgb_run_alpha(buf: PByte; r, g, b: Byte; alpha, n: Integer); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgb_svp_aa (svp: PArtSVP; x0, y0, x1, y1: Integer; fg_color, bg_color: Cardinal; buf: PByte; rowstride: Integer; alphagamma: PArtAlphaGamma); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgb_svp_alpha (svp: PArtSVP; x0, y0, x1, y1: Integer; rgba: Cardinal; buf: PByte; rowstride: Integer; alphagamma: PArtAlphaGamma); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgba_rgba_composite(dst: Cardinal; src: PByte; n: Integer); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgba_fill_run(buf: PByte; r, g, b: Byte; n: Integer); cdecl; external 'libart_lgpl_22.dll';
procedure art_rgba_run_alpha (buf: PByte; r, g, b: Byte; alpha: Integer; n: Integer); cdecl; external 'libart_lgpl_22.dll';
procedure art_svp_render_aa(svp: PArtSVP; x0, y0, x1, y1: Integer; callback: TArtSVPCallback; callback_data: Pointer); cdecl; external 'libart_lgpl_22.dll';
procedure art_gray_svp_aa(svp: PArtSVP; x0, y0, x1, y1: Integer; buf: PByte; rowstride: Integer); cdecl; external 'libart_lgpl_22.dll';

implementation

end.
