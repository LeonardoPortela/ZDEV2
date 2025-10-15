FUNCTION zdocumento_nf_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  ZDOC_REM OPTIONAL
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_DIRECT) TYPE  J_1BDIRECT OPTIONAL
*"     REFERENCE(I_CHECK_AUT) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(E_VBELN) TYPE  ZDOC_REM
*"----------------------------------------------------------------------

  DATA: v_vbeln  TYPE vbfa-vbeln,
        v_vbelv  TYPE vbfa-vbelv,
        v_mjahr  TYPE vbfa-mjahr,
        v_refkey TYPE j_1bnflin-refkey.

  RANGES: lra_docsta FOR j_1bnfe_active-docsta.
  RANGES: lra_cancel FOR j_1bnfe_active-cancel. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

  CLEAR: e_docnum, e_vbeln.

  CHECK ( i_vbeln IS NOT INITIAL ) OR ( i_docnum IS NOT INITIAL ).

  "SD - Geração Ordem Venda sem Deposito US #153341 - WPP --->>>
  IF i_check_aut EQ abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '1' ) TO lra_docsta.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = abap_false ) TO lra_cancel. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
  ENDIF.
  "SD - Geração Ordem Venda sem Deposito US #153341 - WPP <<<---

  IF i_vbeln IS NOT INITIAL. "Numero Remessa

    CASE i_direct.
      WHEN '1'. "Entrada
        SELECT SINGLE a~vbeln a~mjahr
          FROM vbfa AS a INTO (v_vbeln,v_mjahr)
         WHERE a~vbelv    EQ i_vbeln
           AND a~vbtyp_n  EQ 'R'
           AND a~vbtyp_v  EQ 'J'
           AND NOT EXISTS ( SELECT *
                                FROM vbfa AS b
                               WHERE b~vbelv   = a~vbeln
                                 AND b~vbtyp_n = 'h' "estorno
                             ).

        CHECK ( sy-subrc EQ 0 ) AND ( v_vbeln IS NOT INITIAL ).

        CONCATENATE v_vbeln v_mjahr INTO v_refkey.

      WHEN '2'. "Saída

        SELECT SINGLE a~vbeln a~mjahr
          FROM vbfa AS a INTO ( v_vbeln, v_mjahr )
         WHERE a~vbelv    EQ i_vbeln
           AND a~vbtyp_n  EQ 'M'
           AND a~vbtyp_v  EQ 'J'
           AND NOT EXISTS ( SELECT *
                             FROM vbfa AS b
                            WHERE b~vbelv   = a~vbeln
                              AND b~vbtyp_n = 'N' "estorno
                           ).

        CHECK ( sy-subrc EQ 0 ) AND ( v_vbeln IS NOT INITIAL ).

        v_refkey = v_vbeln.
    ENDCASE.

    CHECK v_refkey IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(_wl_lin)
     WHERE refkey = @v_refkey.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(_wl_active)
     WHERE docnum     = @_wl_lin-docnum
       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
       "AND cancel     = @abap_false
        AND cancel     IN @lra_cancel
       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

       "SD - Geração Ordem Venda sem Deposito US #153341 - WPP --->>>
       "AND docsta     = '1'.
       AND docsta     IN @lra_docsta.
    "SD - Geração Ordem Venda sem Deposito US #153341 - WPP <<<---

    CHECK sy-subrc EQ 0.

    e_docnum = _wl_lin-docnum.

  ELSEIF i_docnum IS NOT INITIAL. "Numero Documento Fiscal

    CASE i_direct.
      WHEN '1'. "Entrada
      WHEN '2'. "Saida

        SELECT SINGLE *
          FROM j_1bnfe_active INTO _wl_active
         WHERE docnum     = i_docnum
           "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
           "AND cancel     = abap_false
           AND cancel     IN lra_cancel
           "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

           "SD - Geração Ordem Venda sem Deposito US #153341 - WPP --->>>
           "AND docsta     = '1'.
           AND docsta IN lra_docsta.
        "SD - Geração Ordem Venda sem Deposito US #153341 - WPP <<<---

        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM j_1bnflin INTO _wl_lin
         WHERE docnum = i_docnum.

        CHECK ( sy-subrc EQ 0 ) AND ( _wl_lin-refkey IS NOT INITIAL ).

        v_vbeln = _wl_lin-refkey(10).

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        IF i_check_aut EQ abap_false.
          SELECT SINGLE a~vbelv
            FROM vbfa AS a INTO v_vbelv
           WHERE a~vbeln    EQ v_vbeln
             AND a~vbtyp_n  IN ( 'M' , 'R' ) "Documento Material ou Fatura
             AND a~vbtyp_v  EQ 'J'.
        ELSE.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

          SELECT SINGLE a~vbelv
            FROM vbfa AS a INTO v_vbelv
           WHERE a~vbeln    EQ v_vbeln
             AND a~vbtyp_n  IN ( 'M' , 'R' ) "Documento Material ou Fatura
             AND a~vbtyp_v  EQ 'J'
             AND NOT EXISTS ( SELECT *
                               FROM vbfa AS b
                              WHERE b~vbelv   = a~vbeln
                                AND b~vbtyp_n = 'N' "estorno
                             ).
        ENDIF. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

        CHECK ( sy-subrc EQ 0 ) AND ( v_vbelv IS NOT INITIAL ).

        e_vbeln = v_vbelv. "Atribuir Numero Remessa
    ENDCASE.
  ENDIF.


ENDFUNCTION.
