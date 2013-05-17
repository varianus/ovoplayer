(*
 * copyright (c) 2001 Fabrice Bellard
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

   ===

   revision 12489, 18-mar-2008

   delphi conversion of libavcodec/rational.h

   slightly.fat.hamster@gmail.com, originally by Victor Zinetz

   24-sept-2008

   ===

 *)

unit rational;

interface

uses
  windows;

const
  dll_name = 'avutil-49.dll';

type
  int = integer;

(**
 * Rational number num/den.
 *)
  PAVRational = ^TAVRational;
  TAVRational = record
    num: int; ///< numerator
    den: int; ///< denominator
  end;

(**
 * Compare two rationals.
 * @param a first rational
 * @param b second rational
 * @return 0 if a==b, 1 if a>b and -1 if a<b.
 *)
function av_cmp_q (a: TAVRational; b: TAVRational): int;

(**
 * Rational to double conversion.
 * @param a rational to convert
 * @return (double) a
 *)
function av_q2d (a: TAVRational): double;

(**
 * Reduce a fraction.
 * This is useful for framerate calculations.
 * @param dst_nom destination numerator
 * @param dst_den destination denominator
 * @param nom source numerator
 * @param den source denominator
 * @param max the maximum allowed for dst_nom & dst_den
 * @return 1 if exact, 0 otherwise
 *)
function av_reduce (dst_nom: PInteger; dst_den: PInteger; nom: int64; den: int64; max: int64): int;
  stdcall; external dll_name;

(**
 * Multiplies two rationals.
 * @param b first rational.
 * @param c second rational.
 * @return b*c.
 *)
function av_mul_q (b: TAVRational; c: TAVRational): TAVRational;
  stdcall; external dll_name;

(**
 * Divides one rational by another.
 * @param b first rational.
 * @param c second rational.
 * @return b/c.
 *)
function av_div_q (b: TAVRational; c: TAVRational): TAVRational;
  stdcall; external dll_name;

(**
 * Adds two rationals.
 * @param b first rational.
 * @param c second rational.
 * @return b+c.
 *)
function av_add_q (b: TAVRational; c: TAVRational): TAVRational;
  stdcall; external dll_name;

(**
 * Subtracts one rational from another.
 * @param b first rational.
 * @param c second rational.
 * @return b-c.
 *)
function av_sub_q (b: TAVRational; c: TAVRational): TAVRational;
  stdcall; external dll_name;

(**
 * Converts a double precision floating point number to a rational.
 * @param d double to convert
 * @param max the maximum allowed numerator and denominator
 * @return (AVRational) d.
 *)
function av_d2q (d: double; max: int): TAVRational;
  stdcall; external dll_name;

implementation

function av_cmp_q (a: TAVRational; b: TAVRational): int;
var
  tmp: int64;
begin
  tmp := a.num * b.den - b.num * a.den;

  if tmp > 0 then
    Result := (tmp shr 63) or 1
  else
    Result := 0;
end;

function av_q2d (a: TAVRational): double;
begin
  Result := a.num / a.den;
end;

end.
