*&---------------------------------------------------------------------*
*& Include ZFIR0100_DADOS
*&---------------------------------------------------------------------*

FORM pega_dados .

  CLEAR: it_saida,it_saida[],it_obj_key,it_obj_key[],dtini,dtfim.

  DATA(periodo) = |{ p_ano }{ p_mes }|.


  dtini = |{ periodo }01|.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = dtini
    IMPORTING
      last_day_of_month = dtfim
    EXCEPTIONS
      OTHERS            = 02.

  CALL FUNCTION 'GET_DATA_ZFIR0100'
    EXPORTING
      i_bukrs    = p_emp
      i_gjahr    = p_ano
      i_dtini    = dtini
      i_dtfim    = dtfim
    IMPORTING
      e_zfir0100 = it_selecao.

  IF it_selecao IS NOT INITIAL .

    LOOP AT it_selecao ASSIGNING FIELD-SYMBOL(<get_validacao_obj_key>) GROUP BY <get_validacao_obj_key>-obj_key.

      DATA(obj_key_tmp) = <get_validacao_obj_key>-obj_key.

      SELECT SINGLE a~obj_key,chv~obj_key AS obj_key_chv,err~obj_key AS obj_key_err
      FROM zib_contabil AS a
      LEFT JOIN zib_contabil_chv AS chv ON a~obj_key = chv~obj_key
      LEFT JOIN zib_contabil_ERR AS err ON a~obj_key = err~obj_key
      WHERE a~obj_key = @obj_key_tmp
      INTO @DATA(aux_obj_key).

      IF aux_obj_key IS NOT INITIAL.
        IF aux_obj_key-obj_key_chv IS NOT INITIAL.
          it_obj_key-obj_key = aux_obj_key-obj_key.
          it_obj_key-existe = 'CHV'.
        ELSE.
          IF aux_obj_key-obj_key_err IS NOT INITIAL.
            it_obj_key-obj_key = aux_obj_key-obj_key.
            it_obj_key-existe = 'ERR'.
          ENDIF.
        ENDIF.
      ELSE.
        it_obj_key-obj_key = <get_validacao_obj_key>-obj_key.
        it_obj_key-existe = 'NOT'.
      ENDIF.
      CLEAR: aux_obj_key.
      FREE: aux_obj_key.
      APPEND it_obj_key.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM it_obj_key.

    LOOP AT it_obj_key ASSIGNING FIELD-SYMBOL(<set_alv>).


      READ TABLE it_selecao INTO DATA(wa_selecao) WITH KEY obj_key = <set_alv>-obj_key.

      IF <set_alv>-existe = 'CHV' OR <set_alv>-existe = 'ERR'.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
          EXPORTING
            input  = wa_selecao-belnr
          IMPORTING
            output = it_saida-belnr. " CONVERSION_EXIT_ALPHA_OUTPUT

        SELECT SINGLE * FROM zib_contabil
        WHERE obj_key = @<set_alv>-obj_key
        AND seqitem = ( SELECT MAX( seqitem ) AS seqitem FROM zib_contabil WHERE obj_key = @<set_alv>-obj_key )
        INTO CORRESPONDING FIELDS OF @it_saida.

        IF <set_alv>-existe = 'CHV'.
          it_saida-status = icon_green_light.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @it_saida-obj_key INTO @it_saida-xblnr.
        ELSEIF <set_alv>-existe = 'ERR'.
          it_saida-status = icon_red_light.
        ELSE.
          it_saida-status =  icon_yellow_light.
        ENDIF.

      ELSE.

        SELECT SINGLE *
        FROM acdoca AS a
        WHERE ltrim( a~belnr, '0' ) = @wa_selecao-belnr
        AND a~rbukrs = @wa_selecao-rbukrs
        AND a~gjahr = @wa_selecao-gjahr
        AND substring( a~belnr,1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
        INTO @DATA(WA_acdoca).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
          EXPORTING
            input  = WA_acdoca-belnr
          IMPORTING
            output = WA_acdoca-belnr. " CONVERSION_EXIT_ALPHA_OUTPUT


        it_saida-seqitem = 0.
        it_saida-belnr = WA_acdoca-belnr.
        it_saida-xblnr = abap_false.
        it_saida-status = icon_light_out.
        it_saida-obj_key = <set_alv>-obj_key.
        it_saida-gsber = WA_acdoca-rbusa.
        it_saida-bukrs = WA_acdoca-rbukrs.
        it_saida-bldat = |{ WA_acdoca-bldat+6(2) }.{ WA_acdoca-bldat+4(2) }.{ WA_acdoca-bldat+0(4) }|.
        it_saida-budat = |{ WA_acdoca-budat+6(2) }.{ WA_acdoca-budat+4(2) }.{ WA_acdoca-budat+0(4) }|.
        it_saida-gjahr = WA_acdoca-gjahr.

      ENDIF.

      APPEND it_saida.

      CLEAR: WA_acdoca,wa_selecao.
      FREE: WA_acdoca,wa_selecao.

    ENDLOOP.

  ENDIF.

ENDFORM.
