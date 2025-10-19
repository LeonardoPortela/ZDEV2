function z_les_veiculos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(START_COLUMN) TYPE  SY-CUCOL DEFAULT 29
*"     VALUE(START_ROW) TYPE  SY-CUROW DEFAULT 13
*"     VALUE(END_COLUMN) TYPE  SY-CUCOL DEFAULT 122
*"     VALUE(END_ROW) TYPE  SY-CUROW DEFAULT 21
*"     REFERENCE(P_LIFNR) TYPE  LIFNR OPTIONAL
*"  TABLES
*"      C_XVTTK STRUCTURE  VTTKVB OPTIONAL
*"      C_XVTTS STRUCTURE  VTTSVB OPTIONAL
*"      C_XVTTP STRUCTURE  VTTPVB OPTIONAL
*"      C_PLACA STRUCTURE  ZLESE0032 OPTIONAL
*"----------------------------------------------------------------------

  refresh: ti_xvttk,ti_xvtts,ti_xvttp,ti_placa.
  clear: ti_xvttk,ti_xvtts,ti_xvttp, parceiro_pv.

  if not c_xvttk[] is initial.
    ti_xvttk[] = c_xvttk[].
    ti_xvtts[] = c_xvtts[].
    ti_xvttp[] = c_xvttp[].
  endif.

  if not p_lifnr is initial.
    parceiro_pv = p_lifnr.
  endif.

  call screen 200 starting at start_column start_row
                  ending   at end_column   end_row  .


  if not ti_placa[] is initial.
    c_placa[] = ti_placa[].
  endif.

endfunction.
