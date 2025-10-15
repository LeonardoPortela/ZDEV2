"Name: \PR:SAPMF05A\FO:FCODE_BEARBEITUNG\SE:BEGIN\EI
ENHANCEMENT 0 Z_ENHANCEMENT_F08.
data: t_hkont           type standard table of  rgsb4 with header line,
      mensa(100)        type c,
      xflag(1),
      e_status(1),
      e_messa(64),
      wa_zsaldo_cta_mov type zsaldo_cta_mov.


if xbkpf-bukrs is not initial and xbkpf-budat is not initial.
  call function 'Z_CONTROLE_FECHAMES'
    exporting
      i_bukrs  = xbkpf-bukrs
      i_data   = xbkpf-budat
    importing
      e_status = e_status
      e_messa  = e_messa
    exceptions
      error    = 1
      others   = 2.

  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  if  e_status = 'E'.
    message e000(z01) with e_messa.
  endif.
endif.

"bug 168807 -
if 'FB08' cs sy-tcode.
  if xbkpf-bktxt+0(6) = 'GFFIXO'.
    message e133(zfi).
  endif.
endif.
"bug 168807 -

if 'FB08' cs sy-tcode   and sy-ucomm = 'BU'.
  "
  "não permitir estorno INVOICE COMEX fora da ZFI0017
  data _bsak type bsak.
  data _bkpf type bkpf.
  data _zfit0036 type zfit0036.

*  select SINGLE *
*    from bsak
*    into _BSAK
*    where bukrs = XBKPF-BUKRS
*    and   augbl = XBKPF-STBLG
*    and   gjahr = XBKPF-STJAH
*    and   blart = 'SI'
*    and   bschl in ('31', '34' ). "fornecedores
*
*  IF sy-subrc = 0.
*    MESSAGE E000(Z01) WITH  'Esta compensação é de INVOICE do COMEX.'
*                           ' Estorne pela ZFI0017, INVOICE'
*                           _zfit0036-invoice.
*  ENDIF.

  select single * into wa_zsaldo_cta_mov
    from zsaldo_cta_mov
   where bukrs eq xbkpf-bukrs
     and gjahr eq xbkpf-belnr
     and belnr eq xbkpf-gjahr.

  if sy-subrc is initial.
    message e030(zfi).
  endif.
endif.

ENDENHANCEMENT.
