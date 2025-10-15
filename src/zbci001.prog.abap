*&---------------------------------------------------------------------*
*&  Include           ZBCI001
*&---------------------------------------------------------------------*
tables  sscrfields.

data: s_uname like range of sy-uname with header line.

define grava_range_suname.
  s_uname-sign   = 'I'.
  s_uname-option = 'EQ'.
  s_uname-low    = &1.
  append s_uname.
end-of-definition.



define ini_modo_bi.
  refresh s_uname.
  clear   s_uname.

*> Para incluir novos usuários copiar as 4 linhas abaixo e colocar o
*> usuário desejado. Obs: Lembrar o limite de linhas para range
  grava_range_suname 'BRXS_BASIS'.
  grava_range_suname 'MSANTOS'.
  grava_range_suname 'ABAP'.

*  s_uname-sign   = 'I'.
*  s_uname-option = 'EQ'.
*  s_uname-low    = 'BRXS_BASIS'.
*  append s_uname.
*
*  s_uname-sign   = 'I'.
*  s_uname-option = 'EQ'.
*  s_uname-low    = 'MSANTOS'.
*  append s_uname.
*
*  s_uname-sign   = 'I'.
*  s_uname-option = 'EQ'.
*  s_uname-low    = 'GSANTANA'.
*  append s_uname.

  sscrfields-functxt_01 = 'Modo BI'.

  vg_txt1 = 'Escolha o modo de execução para o Batch Input'.
  vg_txt2 = 'Modo de execução BI'.
end-of-definition.

define check_modo_bi.
  check ( sscrfields-ucomm eq 'FC01' ).

  if ( sy-uname in s_uname ).
    call selection-screen '101' starting at 10 10.
  else.
    message w000 with text-m01.
  endif.
end-of-definition.


selection-screen begin of screen 0101 title vg_txt1 as window.

selection-screen begin of block s1 with frame title vg_txt2.
parameters:     p_mod1  type c default 'N'.
selection-screen end   of block s1.

selection-screen end   of screen 0101.
