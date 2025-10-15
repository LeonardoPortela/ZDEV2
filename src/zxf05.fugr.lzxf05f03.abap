*----------------------------------------------------------------------*
***INCLUDE ZXF05F03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTIR_CPF_INSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LFA1_LIFNR  text
*      -->P_I_LFA1_STCD2  text
*      -->P_I  text
*      -->P_I_LFA1_STCD3  text
*----------------------------------------------------------------------*
FORM f_consistir_cpf_insc  USING    p_but000 TYPE but000
                                    p_lifnr
                                    p_stcd2
                                    p_stcd3
                                    p_regio.  "*-CS2024000622-26.07.2024-JT-#146685

*--> CS1007567/IR102937-->

  IF sy-tcode+3(1) NE '3'.
*<-- CS1007567/IR102937--<


    DATA: v_lifnr    TYPE lfa1-lifnr,
          v_stcd2    TYPE lfa1-stcd2,
          v_stcd3    TYPE lfa1-stcd3,
          v_text1    TYPE spop-textline2,
          v_text2    TYPE spop-textline2,
          v_res      TYPE c LENGTH 1,
          v_res1     TYPE c LENGTH 1,
          v_pergunta TYPE char01.

    CHECK NOT p_stcd2 IS INITIAL.

*-CS2024000622-26.07.2024-JT-#146685-inicio
    SELECT SINGLE valfrom
      FROM setleaf
      INTO @DATA(l_uf_perm_dupl)
     WHERE setname = 'MAGGI_BP_CPF_DUPL'
       AND valfrom = @p_regio .
    IF sy-subrc = 0.
      v_pergunta = abap_true.
    ENDIF.
*-CS2024000622-26.07.2024-JT-#146685-fim

*-CS2024000622-19.09.2024-JT-#152691-inicio
*    SELECT lifnr stcd2 INTO (v_lifnr, v_stcd2)
*      FROM lfa1
*     UP TO 1 ROWS
*     WHERE stcd2 = p_stcd2
*       AND stcd3 = p_stcd3
*       AND lifnr <> p_lifnr
*       AND sperr NE 'X'.
*    ENDSELECT.

    SELECT dfkkbptaxnum~*
      FROM dfkkbptaxnum
     INNER JOIN ibpsupplier   ON ibpsupplier~businesspartner = dfkkbptaxnum~partner
     INNER JOIN lfa1          ON lfa1~lifnr                  = ibpsupplier~supplier
      INTO TABLE @DATA(t_tax)
     WHERE taxnum     = @p_stcd2
       AND taxtype    = 'BR2'
       AND lfa1~sperr = @abap_false
       AND lfa1~loevm = @abap_false
       AND lfa1~nodel = @abap_false.

    SELECT dfkkbptaxnum~*
      FROM dfkkbptaxnum
     INNER JOIN ibpsupplier   ON ibpsupplier~businesspartner = dfkkbptaxnum~partner
     INNER JOIN lfa1          ON lfa1~lifnr                  = ibpsupplier~supplier
 APPENDING TABLE @t_tax
     WHERE taxnum     = @p_stcd3
       AND taxtype    = 'BR3'
       AND lfa1~sperr = @abap_false
       AND lfa1~loevm = @abap_false
       AND lfa1~nodel = @abap_false.

    DATA(t_fornece) = t_tax[].

    DELETE t_fornece WHERE partner = p_but000-partner. "p_lifnr.
    DELETE ADJACENT DUPLICATES FROM t_fornece
                          COMPARING partner.

    LOOP AT t_fornece INTO DATA(w_fornece).
      READ TABLE t_tax INTO DATA(w_tax) WITH KEY partner = w_fornece-partner
                                                 taxtype = 'BR3'
                                                 taxnum  = p_stcd3.
      IF sy-subrc = 0.
        READ TABLE t_tax INTO w_tax WITH KEY partner = w_fornece-partner
                                             taxtype = 'BR2'
                                             taxnum  = p_stcd2.
        IF sy-subrc = 0.
          v_lifnr = w_tax-partner.
          v_stcd2 = w_tax-taxnum.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
*-CS2024000622-19.09.2024-JT-#152691-fim

*   IF sy-subrc = 0.
    IF v_stcd2 IS NOT INITIAL.  "*-CS2024000622-19.09.2024-JT-#152691
*-CS2024000622-26.07.2024-JT-#146685-inicio
      CASE v_pergunta.
        WHEN abap_true.
          CONCATENATE 'Código:' v_lifnr INTO v_text1.
*         CONCATENATE 'CNPJ:' p_stcd2 INTO v_text1.
          v_text2 = 'CNPJ E INSC.ESTADUAL DUPLICADOS!Deseja criar um novo Parceiro de Negócio?'.

          IF sy-batch = abap_off.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                defaultoption = 'N'
                titel         = 'ATENÇÃO'
                textline1     = v_text1
                textline2     = v_text2
              IMPORTING
                answer        = v_res.
          ELSE.
            v_res = 'N'.
          ENDIF.

          IF v_res EQ 'N' OR v_res EQ 'A'.
            MESSAGE e009(zfi) RAISING error. "CPF + Inscrição estadual duplicados !
          ENDIF.

        WHEN abap_false.
          MESSAGE e009(zfi) RAISING error. "CPF + Inscrição estadual duplicados !
      ENDCASE.
*-CS2024000622-26.07.2024-JT-#146685-fim

    ENDIF.
  ENDIF.

ENDFORM.                    " F_CONSISTIR_CPF_INSC
