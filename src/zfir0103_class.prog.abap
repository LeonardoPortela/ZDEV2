CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column sender.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_before_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i09.
  ENDMETHOD.

  METHOD on_after_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i10.
  ENDMETHOD.

  METHOD on_link_click.

    IF sy-subrc = 0.

      IF column = 'STATUS'.

        READ TABLE it_saida INDEX row INTO DATA(ls_saida).

        DATA: msg_status TYPE string.
        CLEAR: msg_status.

        CASE ls_saida-status.
          WHEN icon_red_light.

            CLEAR: lv_message.

            SELECT message FROM zib_contabil_err WHERE obj_key = @ls_saida-obj_key INTO TABLE @it_message.
            SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @ls_saida-obj_key INTO @DATA(aux_message_erro).

            LOOP AT it_message ASSIGNING FIELD-SYMBOL(<msg_erro>).
              CONCATENATE lv_message <msg_erro> INTO lv_message SEPARATED BY space.
            ENDLOOP.

            DATA: dt TYPE string,
                  hr TYPE string.
            CLEAR: dt, hr.
            dt =  aux_message_erro-dt_atualizacao.
            hr =  aux_message_erro-hr_ATUALIZACAO.

            msg_status = |Dt/Hr: { dt } - { hr } Objto KEY: { ls_saida-obj_key } = Obs: { lv_message }!|.
          WHEN icon_yellow_light.
            msg_status = |Objto KEY: { ls_saida-obj_key  } aguardando job da Zib_contabil, Aguarde alguns instantes e Atualize a Grid!|.
          WHEN icon_green_light.
            msg_status = |Objto KEY: { ls_saida-obj_key  } gerado com Sucesso!|.
          WHEN icon_light_out.
            msg_status = |Objto KEY: { ls_saida-obj_key  } disponivel para ser gerado NA Zib_contabil!|.
        ENDCASE.


        CLEAR: lv_message.
        lv_message = |{ msg_status }|.

        MESSAGE lv_message TYPE 'I'.

      ELSEIF column = 'OBJ_KEY'.

        PERFORM get_row.

        FREE: it_acdoca, it_saida2.

        READ TABLE it_saida INTO DATA(get_status) INDEX row.

        IF get_status-status = icon_light_out.

          UNPACK get_status-belnr TO get_status-belnr.

          SELECT *
          FROM acdoca AS a
          WHERE a~belnr = @get_status-belnr
          AND a~rbukrs = @get_status-bukrs
          AND a~gjahr = @get_status-gjahr
          AND substring( a~belnr,1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
          INTO TABLE @it_acdoca.

          LOOP AT it_acdoca ASSIGNING FIELD-SYMBOL(<ajusta_acdoca>).
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
              EXPORTING
                input  = <ajusta_acdoca>-belnr
              IMPORTING
                output = <ajusta_acdoca>-belnr
              . " CONVERSION_EXIT_ALPHA_OUTPUT
          ENDLOOP.

          IF it_acdoca IS NOT INITIAL.

            "DELETE it_acdoca WHERE hsl = 0 AND ksl = 0.
            DELETE it_acdoca WHERE racct = '0000212200'.

            DATA: aux_belnr TYPE belnr_d.
            DATA: aux_xblnr TYPE belnr_d.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
              EXPORTING
                input  = get_status-belnr
              IMPORTING
                output = get_status-belnr
              . " CONVERSION_EXIT_ALPHA_OUTPUT

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
              EXPORTING
                input  = get_status-xblnr
              IMPORTING
                output = get_status-xblnr
              . " CONVERSION_EXIT_ALPHA_OUTPUT


            IF it_acdoca IS NOT INITIAL.

              DATA linha TYPE i.
              CLEAR: linha.

              SORT it_acdoca ASCENDING BY rbukrs gjahr belnr.

              LOOP AT it_acdoca ASSIGNING FIELD-SYMBOL(<acdoca>).
                linha = it_saida2-seqitem.
                linha = linha + 1.
                UNPACK linha TO it_saida2-seqitem.

                CLEAR: aux_belnr,aux_xblnr.

                it_saida2-belnr = <acdoca>-belnr.
                it_saida2-xblnr = abap_false.


                it_saida2-obj_key        = get_status-obj_key.

                it_saida2-status = get_status-status.

                it_saida2-rg_atualizado  = 'N'.
                it_saida2-rldnr          = '50'.


                IF <acdoca>-bschl = '75'. "Valor Negativo
                  it_saida2-bschl = '50'.
                ELSEIF <acdoca>-bschl = '70'. "Valor Negativo
                  it_saida2-bschl = '40'.
                ELSE.
                  it_saida2-bschl = <acdoca>-bschl.
                ENDIF.

                IF <acdoca>-tsl < 0.
                  <acdoca>-tsl = <acdoca>-tsl * -1.
                ENDIF.

                IF <acdoca>-hsl < 0.
                  <acdoca>-hsl = <acdoca>-hsl * -1.
                ENDIF.

                IF <acdoca>-ksl < 0.
                  <acdoca>-ksl = <acdoca>-ksl * -1.
                ENDIF.

                IF <acdoca>-hsl = 0 OR <acdoca>-ksl = 0.
                  it_saida2-waers_i        = 'X'.
                ELSE.
                  it_saida2-waers_i        = <acdoca>-rhcur.
                ENDIF.
                it_saida2-gsber          = <acdoca>-rbusa.
                it_saida2-bukrs          = <acdoca>-rbukrs.
                it_saida2-bldat          = |{ <acdoca>-bldat+6(2) }.{ <acdoca>-bldat+4(2) }.{ <acdoca>-bldat+0(4) }|.
                it_saida2-budat          = |{ <acdoca>-budat+6(2) }.{ <acdoca>-budat+4(2) }.{ <acdoca>-budat+0(4) }|.
                it_saida2-gjahr          = <acdoca>-gjahr.
                it_saida2-monat          = <acdoca>-poper.
                it_saida2-blart          = <acdoca>-blart.
                CONDENSE <acdoca>-racct NO-GAPS.
                UNPACK <acdoca>-racct TO it_saida2-hkont.
                it_saida2-wrbtr          = <acdoca>-tsl.
                it_saida2-waers          = <acdoca>-rwcur.
                it_saida2-sgtxt          = <acdoca>-sgtxt.
                it_saida2-kostl          = <acdoca>-rcntr.
                it_saida2-prctr          = <acdoca>-prctr.
                it_saida2-dmbtr          = <acdoca>-hsl.
                it_saida2-waers_f        = <acdoca>-rkcur.
                it_saida2-dmbe2          = <acdoca>-ksl.

                APPEND it_saida2 TO it_saida2[].

              ENDLOOP.

            ENDIF.

          ENDIF.

          "cl_demo_output=>display( it_saida2[] ).


        ELSE.
          ", @get_status-status AS status
          CLEAR: aux_belnr,aux_xblnr.


          IF get_status-status = icon_green_light.
            aux_belnr = get_status-belnr.
            aux_xblnr = get_status-xblnr.
          ELSEIF get_status-status = icon_red_light.
            aux_belnr = get_status-belnr.
            aux_xblnr = abap_false.
          ENDIF.


          SELECT
         a~obj_key
        ,@aux_xblnr AS xblnr
        ,@aux_belnr AS belnr
        ,a~seqitem
        ,a~bschl
        ,a~gsber
        ,a~bukrs
        ,a~bldat
        ,a~budat
        ,a~gjahr
        ,a~monat
        ,a~blart
        ,a~hkont
        ,a~wrbtr
        ,a~waers
        ,a~sgtxt
        ,a~kostl
        ,a~prctr
        ,a~waers_i
        ,a~dmbtr
        ,a~waers_f
        ,a~dmbe2
        ,a~rg_atualizado
        ,a~rldnr
        ,@get_status-status AS status
            FROM zib_contabil AS a
          WHERE obj_key = @get_status-obj_key
          INTO CORRESPONDING FIELDS OF TABLE @it_saida2[].

          "cl_demo_output=>display( it_saida2[] ).

        ENDIF.

        gr_table2->refresh( ).

      ENDIF.

    ENDIF.

  ENDMETHOD.                    "on_link_click


ENDCLASS.
