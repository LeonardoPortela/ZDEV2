*&---------------------------------------------------------------------*
*& Include ZSD_AJUSTAR_LOTES
*&---------------------------------------------------------------------*

FORM f_ajustar_lotes    USING i_rbkpv STRUCTURE rbkpv
                     CHANGING i_drseg TYPE      mmcr_drseg.

  CHECK i_rbkpv-j_1bnftype = 'ZR'.  "Devolução (saída) à NF-e entrada

  CHECK i_drseg-menge > i_drseg-remng.

  SELECT SINGLE *
    INTO @DATA(_mseg)
    FROM mseg
   WHERE mblnr = @i_drseg-lfbnr
     AND mjahr = @i_drseg-lfgja
     AND zeile = @i_drseg-lfpos.

  CHECK sy-subrc = 0 AND _mseg-charg IS NOT INITIAL.

  SELECT SINGLE chave_nfe
    INTO @DATA(_chave_nfe)
    FROM zib_nfe_dist_ter
   WHERE mblnr = @i_drseg-lfbnr
     AND mjahr = @i_drseg-lfgja.

  CHECK sy-subrc = 0.

  SELECT SINGLE cd_lote_item
    INTO @DATA(_cd_lote_item)
    FROM zib_nfe_dist_lot
   WHERE chave_nfe = @_chave_nfe
     AND mblnr     = @i_drseg-lfbnr
     AND mjahr     = @i_drseg-lfgja
     AND zeile     = @i_drseg-lfpos.

  IF sy-subrc = 0.
    i_drseg-remng = i_drseg-menge.
    i_drseg-refwr = i_drseg-wrbtr.
  ENDIF.

ENDFORM.
