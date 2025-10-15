*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_METHOD
*&---------------------------------------------------------------------*

INITIALIZATION.
  DATA(obj_main)   = NEW zcl_main( ).
  DATA(obj_even)   = NEW zcl_events( ).

  CALL SCREEN '1' STARTING AT 8 8. "#96115  FF  07.03.2023

CLASS zcl_events IMPLEMENTATION.

  METHOD on_dt_ch.
  ENDMETHOD.

  METHOD on_onf4.

    CASE e_fieldname.
      WHEN 'PERNR' OR 'GRUND'.

        wl_help_info =
        VALUE #(
                 call      = COND #( WHEN e_fieldname EQ 'PERNR' THEN 'M'     ELSE 'T' )
                 object    = 'F'
                 program   = sy-repid
                 dynpro    = '100'
                 tabname   = COND #( WHEN e_fieldname EQ 'PERNR' THEN 'RP50G' ELSE 'AFRUD' )
                 fieldname = e_fieldname
                 fieldtype = COND #( WHEN e_fieldname EQ 'PERNR' THEN 'NUMC'  ELSE 'CHAR' )
                 mcobj     = COND #( WHEN e_fieldname EQ 'PERNR' THEN 'PREM'  ELSE '' )
                 spras     = sy-langu
                 menufunct = 'HC'
                 dynpprog  = sy-repid
                 selectart = COND #( WHEN e_fieldname EQ 'PERNR' THEN 'A'     ELSE 'F' )
               ).

        IF e_fieldname NE 'PERNR'.
          tl_dynpselect = VALUE #( ( fldname = 'WERKS' fldinh = obj_main->header-planplant dyfldname = 'AFRUD-WERKS' ) ).
        ENDIF.

        obj_main->help_start( input = es_row_no-row_id field = e_fieldname ).
        obj_main->refresh( ).

      WHEN 'ACTIVITY'.
        obj_main->f4_activity( field = e_fieldname index = es_row_no-row_id ).
      WHEN OTHERS.
        obj_main->code_f4( input = es_row_no-row_id field = e_fieldname ).
    ENDCASE.

    obj_main->add_line( ).
    obj_main->refresh( ).

  ENDMETHOD.

  METHOD on_dt_ch_fs.

    CHECK et_good_cells IS NOT INITIAL.

    CASE et_good_cells[ 1 ]-fieldname.
      WHEN 'PERNR'.
        obj_main->get_pernr( et_good_cells[ 1 ]-row_id ).
      WHEN 'GRUND'.
        obj_main->get_grund( et_good_cells[ 1 ]-row_id ).
      WHEN 'FIN_CONF'.
        obj_main->set_block( ).
      WHEN 'ACTIVITY'.
        obj_main->get_activity(
          input  = et_good_cells[ 1 ]-value
          input1 = et_good_cells[ 1 ]-row_id
        ).
      WHEN OTHERS.

        obj_main->get_desc(
          input  = et_good_cells[ 1 ]-row_id
          input1 = et_good_cells[ 1 ]-value
          field  = et_good_cells[ 1 ]-fieldname
        ).
    ENDCASE.

    obj_main->add_line( ).

    obj_main->refresh( ).

  ENDMETHOD.

  METHOD on_click.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_main IMPLEMENTATION.

  METHOD screen.
    CALL SCREEN '0100'.
  ENDMETHOD.

  METHOD get_header.
    DATA: cont_oper TYPE p DECIMALS 2.
    CLEAR cont_oper.
    CLEAR wa_aufk.

    IF v_ordem IS INITIAL AND v_nota IS INITIAL.
      CLEAR: v_vornr, v_nota.
      FREE: it_niobj, it_nidef, it_nicau, it_nitas, it_nifac, header.
      obj_main->alv2->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      obj_main->alv3->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      obj_main->alv4->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      obj_main->alv5->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      obj_main->alv6->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      MESSAGE i013 DISPLAY LIKE 'E'.
    ELSE.

**  Begin of    #96115  FF
      IF v_nota IS NOT INITIAL.

        DATA(lv_qmnum) = |{ v_nota ALPHA = IN }|.

        SELECT SINGLE * FROM qmel
          INTO @DATA(ls_qmel)
        WHERE qmnum = @lv_qmnum.

        IF sy-subrc = 0.
          DATA(lv_ordem) = ls_qmel-aufnr.
          v_nota_descr = ls_qmel-qmtxt.
        ELSE.
          CLEAR: v_nota_descr, lv_ordem.
        ENDIF.

      ELSE.
        lv_ordem = input.
      ENDIF.

      CHECK lv_ordem IS NOT INITIAL.
** End of FF

      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number        = lv_ordem
        IMPORTING
          es_header     = header
        TABLES
          et_operations = it_operat
          et_olist      = it_olist
          return        = it_return.

      IF header IS INITIAL.

        MESSAGE s000(zppm001) DISPLAY LIKE 'E' WITH 'Ordem não encontrada'.
        return = 4.
*        LEAVE TO CURRENT TRANSACTION.
        LEAVE TO SCREEN 100.

      ELSE.

**  Begin of    #96115  FF 29.03.2023
        READ TABLE it_zpmt0012 TRANSPORTING NO FIELDS WITH KEY iwerk = header-plant.
        IF sy-subrc <> 0.
          MESSAGE s000(zppm001) DISPLAY LIKE 'E' WITH 'Usuário sem permissão para a filial: ' header-plant .
          return = 4.
          LEAVE TO SCREEN 100.
        ENDIF.
** End of FF

        SELECT a~vaplz a~werks a~aufnr a~ktext b~aufpl c~objnr c~vornr c~ltxa1 c~istru " D~AUERU
        FROM aufk AS a
        INNER JOIN afko AS b ON b~aufnr = a~aufnr
        INNER JOIN afvc AS c ON c~aufpl = b~aufpl
        INTO CORRESPONDING FIELDS OF TABLE t_aufk
        WHERE a~aufnr EQ header-orderid
        AND a~autyp EQ '30'
        AND a~phas1 EQ abap_true.

        IF t_aufk IS INITIAL.
          MESSAGE i000(o0) WITH header-orderid 'não esta liberada'.
        ELSE.
          FREE t_aufk.
          SELECT a~vaplz a~werks a~aufnr a~ktext b~aufpl c~objnr c~vornr c~ltxa1 c~istru " D~AUERU
          FROM aufk AS a
          INNER JOIN afko AS b ON b~aufnr = a~aufnr
          INNER JOIN afvc AS c ON c~aufpl = b~aufpl
          INTO CORRESPONDING FIELDS OF TABLE t_aufk
          WHERE a~aufnr EQ header-orderid
          AND a~autyp EQ '30'
          AND c~steus IN ( 'PM01', 'PM02' )
          AND a~phas1 EQ abap_true.

          IF t_aufk IS INITIAL.
            MESSAGE e097(ru) WITH header-orderid.
          ELSE.

            SELECT *
            FROM afru
            INTO CORRESPONDING FIELDS OF TABLE t_afru
            FOR ALL ENTRIES IN t_aufk
              WHERE aufnr EQ t_aufk-aufnr
              AND vornr EQ t_aufk-vornr
              AND aueru EQ abap_true.
            SORT  t_afru ASCENDING BY aufnr vornr.

            LOOP AT t_afru ASSIGNING FIELD-SYMBOL(<w_afru>).
              LOOP AT t_aufk ASSIGNING FIELD-SYMBOL(<w_aufk>) WHERE aufnr = <w_afru>-aufnr
                                                                AND vornr = <w_afru>-vornr.
                IF sy-subrc = 0.
*            IF LINE_EXISTS( T_AUFK[ AUFNR = <W_AFRU>-AUFNR ] ).
                  DELETE  t_aufk INDEX sy-tabix. CONTINUE.
                ENDIF.
              ENDLOOP.
            ENDLOOP.
**  Begin of    #96115  FF
            IF v_istru IS NOT INITIAL.
              READ TABLE it_operat TRANSPORTING NO FIELDS WITH KEY assembly = v_istru.
              IF sy-subrc = 0.
                DELETE it_operat WHERE assembly <> v_istru.
              ELSE.
                MESSAGE i058(00) WITH v_istru space space 'MARA'. "Entrada &1 &2 &3 não existente em &4 (verificar a entrada)
                CLEAR v_istru.
              ENDIF.

              SELECT SINGLE maktx
                FROM makt
                INTO v_maktx
                WHERE matnr = v_istru
                  AND spras = sy-langu.
              IF sy-subrc <> 0.
                CLEAR v_maktx.
              ENDIF.
            ENDIF.
** End of FF
            IF  t_aufk IS INITIAL.
*              MESSAGE e049(ru) WITH header-orderid 'OPERAÇÃO' v_vornr.
            ELSE.
              IF v_vornr IS NOT INITIAL.
                READ TABLE it_operat WITH KEY activity = v_vornr complete = ' ' TRANSPORTING  NO FIELDS.

                IF sy-subrc <> 0.
**  Begin of    #96115  FF
                  CLEAR v_vornr.

                  MESSAGE i000(zppm001) WITH 'Operação não existe ou já foi encerrada.'.
*                  MESSAGE i283(ru) WITH v_vornr header-orderid.

                  DATA(lv_linhas) = lines( it_operat ).
                  IF lv_linhas > 1.
                    CALL SCREEN 0500 STARTING AT 5 5 ENDING AT 86 24.
                  ENDIF.
** End of FF
                ENDIF.

**  Begin of    #96115  FF  29.03.2023
                DATA: catalog_profile TYPE bapi10011e,
                      return1         TYPE bapireturn,
                      codes           TYPE TABLE OF  bapi10011t.


                IF  header-notif_no IS NOT INITIAL.

                  header-notif_no = |{ header-notif_no ALPHA = IN }|.
                  v_nota          = header-notif_no.
                  v_nota_descr    = header-short_text.

                  CALL FUNCTION 'BAPI_SERVNOT_GETCATALOGPROFIL'
                    EXPORTING
                      number          = header-notif_no
                      language        = sy-langu
                    IMPORTING
                      catalog_profile = catalog_profile
                      return          = return1
                    TABLES
                      codes           = codes.

                  codes1 = VALUE #( FOR ls IN codes WHERE ( cat_typ EQ 'B' OR
                                                            cat_typ EQ 'C' OR
                                                            cat_typ EQ '5' OR
                                                            cat_typ EQ '2' OR
                                                            cat_typ EQ 'A' )
                                    ( ls )
                                  ).

                  me->set_operations( ).

                  CHECK me->get_item( header-notif_no ) IS INITIAL.

                  me->set_item( ).

*                  me->add_line( ).
                ELSE.
                  CLEAR: v_nota, v_nota_descr.
                  FREE : it_niobj,it_nidef, it_nicau, it_nitas, it_nifac.
                ENDIF.
** End of FF

              ELSE.
*                LOOP AT t_aufk ASSIGNING <w_aufk>.
*                  IF <w_aufk>-vornr IS NOT INITIAL.
*                    ADD 1 TO cont_oper.
*                  ENDIF.
*                ENDLOOP.
                DELETE it_operat WHERE complete = 'X'.
                lv_linhas = lines( it_operat ).

                IF lv_linhas > 1.
                  CALL SCREEN 0500 STARTING AT 5 5 ENDING AT 86 24.
                ELSE.
                  READ TABLE it_operat INTO DATA(wa_operations) INDEX 1.
                  v_vornr = wa_operations-activity.
                  CLEAR wa_operations.
                ENDIF.

                READ TABLE t_aufk INTO wa_aufk WITH KEY aufnr = header-orderid
                                                        vornr = v_vornr.
                IF sy-subrc = 0.
                  v_ktext = wa_aufk-ktext.
                  v_op_descrip = wa_aufk-ltxa1.

**  Begin of    #96115  FF  29.03.2023
*                  DATA: catalog_profile TYPE bapi10011e,
*                        return1         TYPE bapireturn,
*                        codes           TYPE TABLE OF  bapi10011t.
** End of FF

                  IF  header-notif_no IS NOT INITIAL.

                    header-notif_no = |{ header-notif_no ALPHA = IN }|.
                    v_nota          = header-notif_no.

                    READ TABLE it_olist WITH KEY notif_no = header-notif_no INTO DATA(ls_notif).
                    IF sy-subrc = 0.
                      v_nota_descr    = ls_notif-short_text.
                    ELSE.
                      CLEAR v_nota_descr.
                    ENDIF.

                    CALL FUNCTION 'BAPI_SERVNOT_GETCATALOGPROFIL'
                      EXPORTING
                        number          = header-notif_no
                        language        = sy-langu
                      IMPORTING
                        catalog_profile = catalog_profile
                        return          = return1
                      TABLES
                        codes           = codes.

                    codes1 = VALUE #( FOR ls IN codes WHERE ( cat_typ EQ 'B' OR
                                                              cat_typ EQ 'C' OR
                                                              cat_typ EQ '5' OR
                                                              cat_typ EQ '2' OR
                                                              cat_typ EQ 'A' )
                                      ( ls )
                                    ).

                    me->set_operations( ).

                    CHECK me->get_item( header-notif_no ) IS INITIAL.

                    me->set_item( ).

*                  me->add_line( ).
                  ELSE.
                    CLEAR: v_nota, v_nota_descr.
                    FREE : it_niobj,it_nidef, it_nicau, it_nitas, it_nifac.
                  ENDIF.

                  IF v_vornr IS NOT INITIAL.
                    PERFORM zapontamento.
                  ENDIF.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_item.

    FREE: it_notiteme, it_notcause, it_notactve, it_nottaske.

    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
      EXPORTING
        number             = input
      IMPORTING
        notifheader_export = it_notif
        notifhdtext        = it_notif_h
      TABLES
        notitem            = it_notiteme
        notifcaus          = it_notcause
        notifactv          = it_notactve
        notiftask          = it_nottaske.

    IF it_notif IS INITIAL.
*      MESSAGE i000(o0) WITH 'Nota não encontrada'.
      return = 4.
    ELSE.
      FREE ld_number.

      ld_number = VALUE #( notif_no = it_notif-notif_no
                          equipment = it_notif-equipment
                         notif_type = it_notif-notif_type
                         short_text = it_notif-short_text
                           priotype = it_notif-priotype
                         sys_status = it_notif-sys_status ).



      ld_syststat = VALUE #( langu = 'PT'
                          languiso = ''
                           refdate = sy-datum
                           reftime = sy-uzeit ).

    ENDIF.
  ENDMETHOD.

  METHOD set_operations.

    FREE: it_opera.

    it_opera = VALUE #( FOR ls IN  it_operat
                          (
                            activity             = ls-activity
                            sub_activity         = ls-sub_activity
                            description          = ls-description
                            work_cntr            = ls-work_cntr
                            duration_normal_unit = ls-duration_normal_unit
                            acttype              = ls-acttype
                          )
                      ).

  ENDMETHOD.

  METHOD set_item.

    it_niobj = VALUE #( FOR l1 IN it_notiteme WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l1 ) ) ).
*    LOOP AT it_niobj ASSIGNING FIELD-SYMBOL(<l1>). <l1>-check = abap_true. ENDLOOP.

    it_nidef = VALUE #( FOR l2 IN it_notiteme WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l2 ) ) ).
*    LOOP AT it_nidef ASSIGNING FIELD-SYMBOL(<l2>). <l2>-check = abap_true. ENDLOOP.

    it_nicau = VALUE #( FOR l3 IN it_notcause WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l3 ) ) ).
*    LOOP AT it_nicau ASSIGNING FIELD-SYMBOL(<l3>). <l3>-check = abap_true. ENDLOOP.

    it_nitas = VALUE #( FOR l4 IN it_nottaske WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l4 ) ) ).
*    LOOP AT it_nitas ASSIGNING FIELD-SYMBOL(<l4>). <l4>-check = abap_true. ENDLOOP.

    it_nifac = VALUE #( FOR l5 IN it_notactve WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l5 ) ) ).
*    LOOP AT it_nifac ASSIGNING FIELD-SYMBOL(<l5>). <l5>-check = abap_true. ENDLOOP.

  ENDMETHOD.

  METHOD set_estrutura.

    CHECK me->get_header( input ) IS INITIAL.

  ENDMETHOD.

  METHOD pbo.

**  Begin of    #96115  FF

    IF v_ordem IS NOT INITIAL.
      FREE: it_notiteme, it_notcause, it_notactve, it_nottaske.

      DATA: ls_header     TYPE bapi_alm_order_header_e,
            lt_operations TYPE TABLE OF  bapi_alm_order_operation_e,
            lt_return     TYPE TABLE OF bapiret2.

      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number        = v_ordem
        IMPORTING
          es_header     = ls_header
        TABLES
          et_operations = lt_operations
          return        = lt_return.

      v_quant_oper = lines( lt_operations ).

      CONDENSE v_quant_oper NO-GAPS.
      IF v_quant_oper CO '0123456789'.
      ELSE.
        CLEAR v_quant_oper.
      ENDIF.

      IF v_de IS INITIAL.
        v_de = '0'.
      ENDIF.

      IF v_quant_oper > 0 AND v_de = 0.
        v_de = 1.
      ENDIF.

      IF v_de <> 0.

        IF v_vornr IS INITIAL.
          READ TABLE lt_operations INDEX 1 INTO DATA(wa_op).
          IF sy-subrc = 0.
            v_vornr = wa_op-activity.
            v_op_descrip = wa_op-description.
          ENDIF.
        ENDIF.

*        DATA(lv_counter) = v_vornr+2(1).
        IF v_vornr IS NOT INITIAL.
          READ TABLE lt_operations WITH KEY activity = v_vornr TRANSPORTING NO FIELDS. "identifica em qual linha está a operação.
          IF sy-subrc = 0.
            v_de = sy-tabix.
          ENDIF.
        ENDIF.

        DATA(lv_counter) = v_de.

        IF v_quant_oper = 1.
          lv_counter = 1.
        ENDIF.

*        READ TABLE it_olist WITH KEY counter = lv_counter INTO DATA(wa_olist). "del    FF - 05.10.2023

        READ TABLE lt_operations WITH KEY activity = v_vornr INTO wa_op. "ins    FF - 05.10.2023
        IF sy-subrc = 0.

          READ TABLE it_olist WITH KEY notif_no = wa_op-notif_no INTO DATA(wa_olist). "ins    FF - 05.10.2023
          IF sy-subrc = 0.

            READ TABLE lt_operations
            WITH KEY activity = v_vornr
                     complete = 'X' TRANSPORTING NO FIELDS. "Nota encerrada.
            IF sy-subrc = 0.
              v_de = lv_counter.
              v_nota = wa_olist-notif_no.
              v_nota_descr = wa_olist-short_text.
              PERFORM bloquear_campos_nota.

            ELSE.
              PERFORM desbloquear_campos_nota.
              v_de = lv_counter.
              v_nota = wa_olist-notif_no.
              v_nota_descr = wa_olist-short_text.
            ENDIF.

          ELSE. "ins    FF - 05.10.2023
            CLEAR: v_nota, v_nota_descr. "ins    FF - 05.10.2023
            PERFORM bloquear_campos_nota. "ins    FF - 05.10.2023
          ENDIF.
        ELSE.
          CLEAR: v_nota, v_nota_descr.
*          v_de = lv_counter.
          PERFORM bloquear_campos_nota.
        ENDIF.
      ENDIF.

***      IF vg_enter IS INITIAL
***        AND sy-ucomm <> 'VOLTAR'
***        AND sy-ucomm <> ' '
***        AND sy-ucomm <> 'EXIT'
***        AND sy-ucomm <> 'ESQ'
***        AND sy-ucomm <> 'DIR'. "Executar apenas 1 vez.
***
***        vg_enter = 'X'.
***        SET CURSOR FIELD 'ZEPM_APONTA_CAT_NOTAS-FETXT'.
***        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
***          EXPORTING
***            functioncode           = '=ENTER' "ENTER
***          EXCEPTIONS
***            function_not_supported = 1
***            OTHERS                 = 2.
***      ENDIF.

      PERFORM f_guarda_dados_tela TABLES it_dados_tela.

      CASE sy-ucomm.
        WHEN 'ESQ'. "Botão Esquerda - notas e operações

          PERFORM verifica_campos_vazios CHANGING v_tipo_ordem.
          CHECK vg_erro IS INITIAL.

          v_de = v_de - 1.

          IF v_de < 1.
            v_de = 1.
          ENDIF.

*          READ TABLE it_olist INDEX v_de INTO wa_olist. "del    FF - 05.10.2023
          READ TABLE lt_operations INDEX v_de INTO wa_op. "ins    FF - 05.10.2023
          IF sy-subrc = 0.
            PERFORM desbloquear_campos_nota.

            READ TABLE it_olist WITH KEY notif_no = wa_op-notif_no INTO wa_olist. "ins    FF - 05.10.2023
            IF sy-subrc = 0.

              v_nota = wa_olist-notif_no.
              v_nota_descr = wa_olist-short_text.
            ELSE.
              CLEAR: v_nota, v_nota_descr. "ins    FF - 05.10.2023
              PERFORM bloquear_campos_nota. "ins    FF - 05.10.2023
            ENDIF.
          ELSE.
            CLEAR: v_nota, v_nota_descr.
            PERFORM bloquear_campos_nota.
          ENDIF.

          DATA lv_oper(4) TYPE n.
*          lv_oper = v_vornr - 10.
          READ TABLE lt_operations INTO wa_op INDEX v_de.
          lv_oper = wa_op-activity.
          IF lv_oper < 0010.
            lv_oper = 0010.
          ENDIF.

          READ TABLE lt_operations WITH KEY activity = lv_oper INTO DATA(wa_oper) .
          IF sy-subrc = 0 AND v_nota IS NOT INITIAL.
            PERFORM desbloquear_campos_nota.
            v_vornr = lv_oper.
            v_op_descrip = wa_oper-description.

            IF wa_oper-complete = 'X'.
              PERFORM bloquear_campos_nota.
              MESSAGE s000(o0) WITH 'A operação' wa_oper-activity 'já foi encerrada.' DISPLAY LIKE 'E'.
            ENDIF.
          ELSE.
            v_vornr = lv_oper.
            v_op_descrip = wa_oper-description.
            PERFORM bloquear_campos_nota.
          ENDIF.

          PERFORM f_guarda_dados_tela TABLES it_dados_tela.
          PERFORM f_le_texto_longo_notif USING v_nota abap_false.
          PERFORM f_set_long_text. "FF #96115  22.06
          CLEAR zepm_aponta_cat_notas.

        WHEN 'DIR'. "Botão Direita - notas e operações

          PERFORM verifica_campos_vazios CHANGING v_tipo_ordem.
          CHECK vg_erro IS INITIAL.

          v_de = v_de + 1.
          IF v_de > v_quant_oper.
            v_de = v_quant_oper.
          ENDIF.
*          READ TABLE it_olist INDEX v_de INTO wa_olist.    "del    FF - 05.10.2023
          READ TABLE lt_operations INDEX v_de INTO wa_op.    "ins    FF - 05.10.2023
          IF sy-subrc = 0.
            PERFORM desbloquear_campos_nota.
            READ TABLE it_olist WITH KEY notif_no = wa_op-notif_no INTO wa_olist. "ins    FF - 05.10.2023
            IF sy-subrc = 0.
              v_nota = wa_op-notif_no.
              v_nota_descr = wa_olist-short_text.
            ELSE.
              CLEAR: v_nota, v_nota_descr. "ins    FF - 05.10.2023
              PERFORM bloquear_campos_nota. "ins    FF - 05.10.2023
            ENDIF.
          ELSE.
            CLEAR: v_nota, v_nota_descr.
            PERFORM bloquear_campos_nota.
          ENDIF.

*          lv_oper = v_vornr + 10.
          READ TABLE lt_operations INTO wa_op INDEX v_de.
          lv_oper = wa_op-activity.

          SORT lt_operations DESCENDING BY activity.
          CLEAR wa_oper.
          READ TABLE lt_operations INDEX 1 INTO wa_oper.
          IF lv_oper > wa_oper-activity.
            lv_oper = wa_oper-activity.
          ENDIF.

          CLEAR wa_oper.
          READ TABLE lt_operations WITH KEY activity = lv_oper INTO wa_oper.
          IF sy-subrc = 0 AND v_nota IS NOT INITIAL.
            PERFORM desbloquear_campos_nota.
            v_vornr = lv_oper.
            v_op_descrip = wa_oper-description.
            IF wa_oper-complete = 'X'.
              PERFORM bloquear_campos_nota.
              MESSAGE s000(o0) WITH 'A operação' wa_oper-activity 'já foi encerrada.' DISPLAY LIKE 'E'.
            ENDIF.
          ELSE.
            v_vornr = lv_oper.
            v_op_descrip = wa_oper-description.
            PERFORM bloquear_campos_nota.
          ENDIF.

          PERFORM f_guarda_dados_tela TABLES it_dados_tela.
          PERFORM f_le_texto_longo_notif USING v_nota abap_false.
          PERFORM f_set_long_text. "FF #96115  22.06
          CLEAR zepm_aponta_cat_notas.

        WHEN OTHERS.
      ENDCASE.

      CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
        EXPORTING
          number             = v_nota
        IMPORTING
          notifheader_export = it_notif
          notifhdtext        = it_notif_h
        TABLES
          notitem            = it_notiteme
          notifcaus          = it_notcause
          notifactv          = it_notactve
          notiftask          = it_nottaske.

      DELETE it_notiteme WHERE delete_flag IS NOT INITIAL.
      DELETE it_notcause WHERE delete_flag IS NOT INITIAL.
      DELETE it_notactve WHERE delete_flag IS NOT INITIAL.
      DELETE it_nottaske WHERE delete_flag IS NOT INITIAL.

*      SORT it_notiteme BY item_key DESCENDING. " RJF Rollback PRD - Bug Solto 146832
      READ TABLE it_notiteme INDEX 1 INTO DATA(ls_notitem).
      IF sy-subrc <> 0.
        CLEAR ls_notitem.
      ENDIF.

*      SORT it_notcause BY item_key DESCENDING. " RJF Rollback PRD - Bug Solto 146832
      READ TABLE it_notcause INDEX 1 INTO DATA(ls_notcause).
      IF sy-subrc <> 0.
        CLEAR ls_notcause.
      ENDIF.

*      SORT it_notactve  BY act_key DESCENDING. " RJF Rollback PRD - Bug Solto 146832
      READ TABLE it_notactve INDEX 1 INTO DATA(ls_notactve).
      IF sy-subrc <> 0.
        CLEAR ls_notactve.
      ENDIF.

      zepm_aponta_cat_notas-notifno = v_nota.

      READ TABLE it_dados_tela INTO zepm_aponta_cat_notas WITH KEY notifno = v_nota.

      IF zepm_aponta_cat_notas-otgrp IS INITIAL.
        zepm_aponta_cat_notas-otgrp   = ls_notitem-dl_codegrp.
      ENDIF.

      IF zepm_aponta_cat_notas-oteil IS INITIAL.
        zepm_aponta_cat_notas-oteil   = ls_notitem-dl_code.
      ENDIF.

      IF zepm_aponta_cat_notas-txtcdot IS INITIAL.
        zepm_aponta_cat_notas-txtcdot = ls_notitem-txt_objptcd.
      ENDIF.

      IF zepm_aponta_cat_notas-fegrp IS INITIAL.
        zepm_aponta_cat_notas-fegrp   = ls_notitem-d_codegrp.
      ENDIF.

      IF zepm_aponta_cat_notas-fecod IS INITIAL.
        zepm_aponta_cat_notas-fecod   = ls_notitem-d_code.
      ENDIF.

      IF zepm_aponta_cat_notas-txtcdfe IS INITIAL.
        zepm_aponta_cat_notas-txtcdfe = ls_notitem-txt_probcd.
      ENDIF.

      IF zepm_aponta_cat_notas-fetxt IS INITIAL.
        zepm_aponta_cat_notas-fetxt   = ls_notitem-descript.
      ENDIF.

      IF zepm_aponta_cat_notas-urgrp IS INITIAL.
        zepm_aponta_cat_notas-urgrp   = ls_notcause-cause_codegrp.
      ENDIF.

      IF zepm_aponta_cat_notas-urcod    IS INITIAL.
        zepm_aponta_cat_notas-urcod   = ls_notcause-cause_code.
      ENDIF.

      IF zepm_aponta_cat_notas-txtcdur IS INITIAL.
        zepm_aponta_cat_notas-txtcdur = ls_notcause-txt_causecd.
      ENDIF.

      IF zepm_aponta_cat_notas-urtxt IS INITIAL.
        zepm_aponta_cat_notas-urtxt   = ls_notcause-causetext.
      ENDIF.

      IF zepm_aponta_cat_notas-mngrp IS INITIAL.
        zepm_aponta_cat_notas-mngrp   = ls_notactve-act_codegrp.
      ENDIF.

      IF zepm_aponta_cat_notas-mncod IS INITIAL.
        zepm_aponta_cat_notas-mncod   = ls_notactve-act_code.
      ENDIF.

      IF zepm_aponta_cat_notas-txtcdma IS INITIAL.
        zepm_aponta_cat_notas-txtcdma = ls_notactve-txt_actcd.
      ENDIF.

      IF zepm_aponta_cat_notas-matxt IS INITIAL.
        zepm_aponta_cat_notas-matxt   = ls_notactve-acttext.
      ENDIF.

      IF zepm_aponta_cat_notas-oteil IS NOT INITIAL AND
         zepm_aponta_cat_notas-fecod IS NOT INITIAL AND
         zepm_aponta_cat_notas-urcod IS NOT INITIAL.

        LOOP AT SCREEN.
          CASE  screen-group1.
            WHEN 'NOT'. "Notas
              screen-input = 0. "fechar campos para edição
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      IF zepm_aponta_cat_notas IS NOT INITIAL.
        READ TABLE it_aponta_notas
         WITH KEY notifno = v_nota TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO it_aponta_notas ASSIGNING FIELD-SYMBOL(<fs_aponta_notas>).
          zepm_aponta_cat_notas-notifno = v_nota.
          <fs_aponta_notas> = zepm_aponta_cat_notas.
        ELSE.
          MODIFY it_aponta_notas FROM zepm_aponta_cat_notas INDEX sy-tabix.
        ENDIF.
      ENDIF.

      IF v_nota IS NOT INITIAL AND
         sy-ucomm <> 'DIR'     AND
         sy-ucomm <> 'ESQ'.
        PERFORM f_le_texto_longo_notif USING v_nota abap_false.
      ENDIF.

    ENDIF.
** End of FF

    it_ucomm =  COND #(
      WHEN sy-ucomm EQ 'EDIT'
                          THEN VALUE #(
                                        ( ucomm = 'SAVE'    )
                                        ( ucomm = 'ENCORD'  )
                                        ( ucomm = 'ENCONO'  )
*                                        ( ucomm = 'DEL'     )                      "FF #96115
                         ) ELSE VALUE #(
                                        ( ucomm = 'APONTAR' )
                                        ( ucomm = 'EDIT'    ) ) ).

    IF obj_main->header-orderid IS INITIAL.
      DATA: w_ucomm TYPE sy-ucomm.

      w_ucomm = 'APONTAR'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.

      w_ucomm = 'DEL'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.
    ENDIF.

    IF obj_main->header-notif_no IS INITIAL.
      w_ucomm = 'EDIT'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.
    ENDIF.

**  Begin of   "FF #96115
**    IF it_aponta IS INITIAL.
**      w_ucomm = 'DEL'.
**      APPEND w_ucomm TO it_ucomm.
**      CLEAR w_ucomm.
**    ENDIF.
** End of FF

    SET PF-STATUS 'PF0100'  EXCLUDING it_ucomm.
    SET TITLEBAR 'TI0100'.

    me->set_block( ).
    me->set_alv1( ).

    IF v_ordem IS NOT INITIAL.
      IF sy-ucomm = 'REFRESH'.
        CLEAR: it_timetickets[], it_timetickets.
      ENDIF.
      IF it_timetickets IS INITIAL.
        IF sy-ucomm = 'ENTER' OR
           sy-ucomm = 'V_CONFIRMA' OR ( w_cursor_field = 'V_VORNR' AND sy-ucomm = 'DOUBLE' ).
          PERFORM zapontamento.
        ENDIF.
      ELSEIF w_cursor_field = 'V_VORNR' AND sy-ucomm = 'DOUBLE'.
        CLEAR: it_timetickets[], it_timetickets.
        PERFORM zapontamento.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_layout.

    FREE: layout, variant, stable, it_exc.

    layout = VALUE #(
                     zebra      = abap_false
                     no_rowins  = abap_true
                     stylefname = 'ESTILO'
                     sel_mode   = 'A'                       "FF #96115

                    ).

    variant = VALUE #(
                      report = sy-repid
                     ).

    stable = VALUE #(
                     row = abap_true
                     col = abap_true
                    ).

    it_exc = VALUE #(
                     ( cl_gui_alv_grid=>mc_fc_excl_all )
                    ).

  ENDMETHOD.

  METHOD set_fcat.

    FREE fcat.

    ASSIGN input TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA str TYPE (<fs_str>).

    fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

    DELETE fcat WHERE fieldname EQ 'ESTILO'.
**  Begin of   "FF #96115
    DELETE fcat WHERE fieldname EQ 'FIN_CONF'."Conf.final
    DELETE fcat WHERE fieldname EQ 'GRUND'. "Causa
    DELETE fcat WHERE fieldname EQ 'GRDTX'. "Descrição Causa
    DELETE fcat WHERE fieldname EQ 'ACTIVITY'. "Operação
    DELETE fcat WHERE fieldname EQ 'SUB_ACTIVITY'. "Descrição Causa
    DELETE fcat WHERE fieldname EQ 'MNGRP'. "Grp Códigos
** End of FF

    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fcat>).
*      <FCAT>-EDIT = ABAP_TRUE.

      CASE <fcat>-fieldname.
        WHEN 'DESCRIPTION' OR 'KTEXT' OR 'SNAME' OR 'GRDTX'.
          <fcat>-outputlen = '20'.
        WHEN 'PERNR' OR 'GRUND' OR 'ACTIVITY'.
*          <fcat>-f4availabl = abap_true.
*          <FCAT>-EDIT = ABAP_TRUE.
          <fcat>-outputlen = <fcat>-outputlen + 1.
        WHEN 'ISDD' OR 'ISDZ' OR 'IEDD' OR 'IEDZ'.
*          <fcat>-f4availabl = abap_true.
*          <fcat>-ref_table = <fcat>-tabname.
*          <fcat>-ref_field = <fcat>-fieldname.
*          <FCAT>-EDIT = ABAP_TRUE.
        WHEN 'SUB_ACTIVITY'.
*          <FCAT>-EDIT = ABAP_FALSE.
        WHEN 'TXT_OBJPTCD' OR 'TXT_PROBCD' OR 'TXT_CAUSECD' OR 'TXT_TASKCD' OR 'TXT_ACTCD'.
*          <FCAT>-EDIT = ABAP_FALSE.
        WHEN 'ACTTYPE' OR 'CHECK'.
          <fcat>-no_out = abap_true.
        WHEN 'FIN_CONF' OR 'CLEAR_RES'.
          <fcat>-checkbox = abap_true.
          <fcat>-just = 'C'.
*          <FCAT>-EDIT = ABAP_TRUE.
          <fcat>-outputlen = '4'.
        WHEN OTHERS.
          <fcat>-outputlen = <fcat>-outputlen + 1.
*          <FCAT>-EDIT = ABAP_TRUE.
      ENDCASE.

      IF <fcat>-fieldname CS 'CAT_TYP' OR <fcat>-fieldname CS 'SORT_NO' OR <fcat>-fieldname CS '_KEY'.
        IF <fcat>-fieldname  NE 'ITEM_KEY'.
*          <FCAT>-NO_OUT = ABAP_TRUE.
        ENDIF.
      ENDIF.

      IF <fcat>-fieldname CS 'TEXT' OR <fcat>-fieldname CS 'TXT_'  OR <fcat>-fieldname EQ 'DESCRIPT'.
        IF <fcat>-fieldname EQ 'TXT_PROBCD'.
          <fcat>-outputlen = '56'.
*          <FCAT>-EDIT = ABAP_FALSE.
        ELSE.
          <fcat>-outputlen = '28'.
        ENDIF.
      ENDIF.

      IF <fcat>-fieldname CS 'CODE'.
*        <fcat>-f4availabl = abap_true.
*        <FCAT>-EDIT = ABAP_TRUE.
      ENDIF.

**  Begin of   "FF #96115
      <fcat>-f4availabl = abap_false. "Desabilitar matchcode
** End of FF


    ENDLOOP.

  ENDMETHOD.

  METHOD set_block.

    IF me->acao EQ 'EDIT'.
      status = ativo.
    ELSE.
      status = inativo.
    ENDIF.

    LOOP AT it_opera ASSIGNING FIELD-SYMBOL(<opera>).
      FREE: estilo.

      estilo = VALUE #(
                         ( fieldname = 'SUB_ACTIVITY'                style = inativo )
                         ( fieldname = 'DESCRIPTION'                 style = inativo )
                         ( fieldname = 'SNAME'                       style = inativo )
                         ( fieldname = 'GRDTX'                       style = inativo )

                         ( fieldname = 'ACTIVITY'                    style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'WORK_CNTR'                   style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'PERNR'                       style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'ISDD'                        style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'ISDZ'                        style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'IEDD'                        style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'IEDZ'                        style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'AFRUD'                       style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'GRUND'                       style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'DURATION_NORMAL_UNIT'        style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'FIN_CONF'                    style = COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) )
                         ( fieldname = 'CLEAR_RES'                   style = COND #( WHEN <opera>-fin_conf EQ abap_false THEN inativo ELSE ativo ) )
                      ).

      <opera>-estilo = estilo.

      IF <opera>-fin_conf EQ abap_false.
*        <OPERA>-CLEAR_RES = ABAP_FALSE.
      ENDIF.

    ENDLOOP.

    LOOP AT it_niobj ASSIGNING FIELD-SYMBOL(<niobj>).
      FREE: estilo.
      estilo = VALUE #(
                        ( fieldname = 'TXT_OBJPTCD' style = inativo )

                        ( fieldname = 'ITEM_KEY'    style = COND #( WHEN <niobj>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'DL_CODEGRP'  style = COND #( WHEN <niobj>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'DL_CODE'     style = COND #( WHEN <niobj>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'DESCRIPT'    style = COND #( WHEN <niobj>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                      ).
      <niobj>-estilo = estilo.
    ENDLOOP.

    LOOP AT it_nidef ASSIGNING FIELD-SYMBOL(<nidef>).
      FREE: estilo.
      estilo = VALUE #(
                        ( fieldname = 'TXT_PROBCD' style = inativo )
                        ( fieldname = 'ITEM_KEY'   style = COND #( WHEN <nidef>-check IS NOT INITIAL THEN status  ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'D_CODEGRP'  style = COND #( WHEN <nidef>-check IS NOT INITIAL THEN status  ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'D_CODE'     style = COND #( WHEN <nidef>-check IS NOT INITIAL THEN status  ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                      ).
      <nidef>-estilo = estilo.
    ENDLOOP.

    LOOP AT it_nicau ASSIGNING FIELD-SYMBOL(<nicau>).
      FREE: estilo.
      estilo = VALUE #(
                        ( fieldname = 'TXT_CAUSECD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'      style = COND #( WHEN <nicau>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'CAUSE_CODEGRP' style = COND #( WHEN <nicau>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'CAUSE_CODE'    style = COND #( WHEN <nicau>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'CAUSETEXT'     style = COND #( WHEN <nicau>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                      ).
      <nicau>-estilo = estilo.
    ENDLOOP.

    LOOP AT it_nitas ASSIGNING FIELD-SYMBOL(<nitas>).
      FREE: estilo.
      estilo = VALUE #(
                        ( fieldname = 'TXT_TASKCD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'     style = COND #( WHEN <nitas>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'TASK_CODEGRP' style = COND #( WHEN <nitas>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'TASK_CODE'    style = COND #( WHEN <nitas>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'TASK_TEXT'    style = COND #( WHEN <nitas>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                      ).
      <nitas>-estilo = estilo.
    ENDLOOP.

    LOOP AT it_nifac ASSIGNING FIELD-SYMBOL(<nifac>).
      FREE: estilo.
      estilo = VALUE #(
                        ( fieldname = 'TXT_ACTCD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'    style = COND #( WHEN <nifac>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'ACT_CODEGRP' style = COND #( WHEN <nifac>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'ACT_CODE'    style = COND #( WHEN <nifac>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                        ( fieldname = 'ACTTEXT'     style = COND #( WHEN <nifac>-check IS NOT INITIAL THEN status ELSE COND #( WHEN status EQ inativo THEN ativo ELSE inativo ) ) )
                      ).
      <nifac>-estilo = estilo.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_alv1.

    me->set_fcat( 'ZOPERATIONS' ).

    IF cont1 IS INITIAL.

      CREATE OBJECT cont1
        EXPORTING
          container_name = 'CC_01'.

      CREATE OBJECT alv1
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont1
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( 'CC_01' ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'PERNR' register = abap_true getbefore = abap_true )
              ( fieldname = 'GRUND' register = abap_true getbefore = abap_true )
              ( fieldname = 'ACTIVITY' register = abap_true getbefore = abap_true )
             ).

      alv1->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv1,
                   obj_even->on_dt_ch_fs FOR alv1,
                   obj_even->on_onf4 FOR alv1,
                   obj_even->on_dt_ch FOR alv1.

      alv1->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
*         IT_OUTTAB                     = IT_OPERA
          it_outtab                     = it_aponta
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      alv1->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      alv1->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv1->refresh_table_display( is_stable = stable ).
    ENDIF.

    me->set_alv2( ).
    me->set_alv3( ).
    me->set_alv4( ).
    me->set_alv5( ).
    me->set_alv6( ).

  ENDMETHOD.

  METHOD set_alv2.

    me->set_fcat( 'ZNOTITEMOBJ' ).

    IF cont2 IS INITIAL.

      CREATE OBJECT cont2
        EXPORTING
          container_name = 'CC_02'.

      CREATE OBJECT alv2
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont2
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'DL_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'DL_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv2->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv2,
                   obj_even->on_dt_ch_fs FOR alv2,
                   obj_even->on_onf4 FOR alv2,
                   obj_even->on_dt_ch FOR alv2.

      alv2->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
          it_outtab                     = it_niobj
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      CALL METHOD alv2->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      CALL METHOD alv2->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv2->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.

  ENDMETHOD.

  METHOD set_alv3.

    me->set_fcat( 'ZNOTITEMDEF' ).

    IF cont3 IS INITIAL.

      CREATE OBJECT cont3
        EXPORTING
          container_name = 'CC_03'.

      CREATE OBJECT alv3
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont3
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'D_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'D_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv3->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv3,
                   obj_even->on_dt_ch_fs FOR alv3,
                   obj_even->on_onf4 FOR alv3,
                   obj_even->on_dt_ch FOR alv3.

      alv3->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
          it_outtab                     = it_nidef
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      CALL METHOD alv3->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      CALL METHOD alv3->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv3->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.

  ENDMETHOD.

  METHOD set_alv4.

    me->set_fcat( 'ZNOTIFCAUS' ).

    IF cont4 IS INITIAL.

      CREATE OBJECT cont4
        EXPORTING
          container_name = 'CC_04'.

      CREATE OBJECT alv4
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont4
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'CAUSE_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'CAUSE_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv4->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv4,
                   obj_even->on_dt_ch_fs FOR alv4,
                   obj_even->on_onf4 FOR alv4,
                   obj_even->on_dt_ch FOR alv4.

      alv4->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
          it_outtab                     = it_nicau
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      CALL METHOD alv4->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      CALL METHOD alv4->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv4->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.

  ENDMETHOD.

  METHOD set_alv5.

    me->set_fcat( 'ZNOTIFTASK' ).

    IF cont5 IS INITIAL.

      CREATE OBJECT cont5
        EXPORTING
          container_name = 'CC_05'.

      CREATE OBJECT alv5
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont5
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'TASK_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'TASK_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv5->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv5,
                   obj_even->on_dt_ch_fs FOR alv5,
                   obj_even->on_onf4 FOR alv5,
                   obj_even->on_dt_ch FOR alv5.

      alv5->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
          it_outtab                     = it_nitas
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      CALL METHOD alv5->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      CALL METHOD alv5->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv5->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.

  ENDMETHOD.

  METHOD set_alv6.

    me->set_fcat( 'ZNOTIFACTV' ).

    IF cont6 IS INITIAL.

      CREATE OBJECT cont6
        EXPORTING
          container_name = 'CC_06'.

      CREATE OBJECT alv6
        EXPORTING
          i_shellstyle    = 0
          i_parent        = cont6
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      CREATE OBJECT obj_even.

      lt_f4 =
      VALUE #(
              ( fieldname = 'ACT_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'ACT_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv6->register_f4_for_fields( it_f4 = lt_f4[] ).

      SET HANDLER: obj_even->on_click FOR alv6,
                   obj_even->on_dt_ch_fs FOR alv6,
                   obj_even->on_onf4 FOR alv6,
                   obj_even->on_dt_ch FOR alv6.

      alv6->set_table_for_first_display(
        EXPORTING
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        CHANGING
          it_outtab                     = it_nifac
          it_fieldcatalog               = fcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4
      ).

      CALL METHOD alv6->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      CALL METHOD alv6->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ELSE.
      alv6->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.

  ENDMETHOD.

  METHOD get_pernr.
    DATA(pernr) = it_opera[ input ]-pernr.
    SELECT SINGLE sname FROM pa0001 INTO return WHERE pernr EQ pernr.
    it_opera[ input ]-sname = return.
  ENDMETHOD.

  METHOD get_grund.

    DATA(grund) = it_opera[ input ]-grund.

    SELECT SINGLE grdtx
      FROM trugt
        INTO return
        WHERE grund EQ grund
          AND werks EQ header-planplant
    AND spras EQ sy-langu.

    it_opera[ input ]-grdtx = return.

  ENDMETHOD.

  METHOD get_activity.

    TRY .
        DATA(zapontat) = it_operat[ activity = input ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    it_opera[ input1 ]-activity    = zapontat-activity.
    it_opera[ input1 ]-description = zapontat-description.
    it_opera[ input1 ]-work_cntr   = zapontat-work_cntr.

  ENDMETHOD.

  METHOD get_desc.

    me->get_cat_code( field = field index = input ).

    it_code = VALUE #( FOR ls IN codes1 WHERE ( cat_typ    EQ me->at_cat_typ AND code_group EQ me->at_code_group )
                         (
                           code_group = ls-code_group
                           code = ls-code
                           shorttxtcd = ls-shorttxtcd )
                         ).
    SORT it_code.
    DELETE ADJACENT DUPLICATES FROM it_code COMPARING ALL FIELDS.
    me->at_code = input1.
    me->set_code( field = field index = input ).
    me->set_txt( field = field index = input ).

  ENDMETHOD.

  METHOD f4_activity.

    it_f4activity = VALUE #( FOR ls IN it_operat ( CORRESPONDING #( ls ) ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = field
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = '100'
      TABLES
        value_tab       = it_f4activity
        return_tab      = tl_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    TRY.
        obj_main->get_activity(
          input  = tl_return[ 1 ]-fieldval
          input1 = index
        ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


  ENDMETHOD.

  METHOD refresh.
    IF alv1 IS NOT INITIAL.
      alv1->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
    IF alv2 IS NOT INITIAL.
      alv2->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
    IF alv3 IS NOT INITIAL.
      alv3->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
    IF alv4 IS NOT INITIAL.
      alv4->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
    IF alv5 IS NOT INITIAL.
      alv5->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
    IF alv6 IS NOT INITIAL.
      alv6->refresh_table_display( EXPORTING is_stable = stable ).
    ENDIF.
  ENDMETHOD.

  METHOD help_start.

    CALL FUNCTION 'HELP_START'
      EXPORTING
        help_infos   = wl_help_info
      IMPORTING
        select_value = wl_selected
      TABLES
        dynpselect   = tl_dynpselect
        dynpvaluetab = tl_dynpvaluetab.

    IF wl_selected IS NOT INITIAL.

      CASE field.
        WHEN 'PERNR'.
          it_opera[ input ]-pernr = wl_selected.
          me->get_pernr( input ).
        WHEN 'GRUND'.
          it_opera[ input ]-grund = wl_selected.
          me->get_grund( input ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD code_f4.

    me->get_cat_code( field = field index = input ).

    IF field CS 'CODEGRP'.

      it_code_group = VALUE #( FOR ls IN codes1 WHERE ( cat_typ EQ me->at_cat_typ )
                                  ( code_group = ls-code_group shorttxtgr = ls-shorttxtgr )
                             ).
      SORT it_code_group.
      DELETE ADJACENT DUPLICATES FROM it_code_group COMPARING ALL FIELDS.
      me->set_f4( table = it_code_group field = 'CODE_GROUP' index = input ).
      me->set_code_group( field = field index = input ).

    ELSE.

      it_code = VALUE #( FOR ls IN codes1 WHERE ( cat_typ    EQ me->at_cat_typ AND
                                                  code_group EQ me->at_code_group
                                                )
                                  ( code_group = ls-code_group code = ls-code shorttxtcd = ls-shorttxtcd )
                       ).
      SORT it_code.
      DELETE ADJACENT DUPLICATES FROM it_code COMPARING ALL FIELDS.
      me->set_f4( table = it_code field = 'CODE' index = input ).
      me->set_code( field = field index = input ).
      me->set_txt( field = field index = input ).

    ENDIF.

    me->refresh( ).

  ENDMETHOD.

  METHOD set_f4.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = field
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = '100'
      TABLES
        value_tab       = table
        return_tab      = tl_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    TRY .
        me->at_code = tl_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR me->at_code.
    ENDTRY.


  ENDMETHOD.

  METHOD get_cat_code.

    CASE field.
      WHEN 'DL_CODE'    OR 'DL_CODEGRP'   .
        me->at_cat_typ = 'B'.
        me->at_code_group = it_niobj[ index ]-dl_codegrp.
      WHEN 'D_CODE'     OR 'D_CODEGRP'    .
        me->at_cat_typ = 'C'.
        me->at_code_group = it_nidef[ index ]-d_codegrp.
      WHEN 'CAUSE_CODE' OR 'CAUSE_CODEGRP'.
        me->at_cat_typ = '5'.
        me->at_code_group = it_nicau[ index ]-cause_codegrp.
      WHEN 'TASK_CODE'  OR 'TASK_CODEGRP' .
        me->at_cat_typ = '2'.
        me->at_code_group = it_nitas[ index ]-task_codegrp.
      WHEN 'ACT_CODE'   OR 'ACT_CODEGRP'  .
        me->at_cat_typ = 'A'.
        me->at_code_group = it_nifac[ index ]-act_codegrp.
    ENDCASE.

  ENDMETHOD.

  METHOD set_code.
    CASE field.
      WHEN 'DL_CODE'    . it_niobj[ index ]-dl_code     = me->at_code.
      WHEN 'D_CODE'     . it_nidef[ index ]-d_code      = me->at_code.
      WHEN 'CAUSE_CODE' . it_nicau[ index ]-cause_code  = me->at_code.
      WHEN 'TASK_CODE'  . it_nitas[ index ]-task_code   = me->at_code.
      WHEN 'ACT_CODE'   . it_nifac[ index ]-act_code    = me->at_code.
    ENDCASE.
  ENDMETHOD.

  METHOD set_code_group.
    CASE field.
      WHEN 'DL_CODEGRP'    . it_niobj[ index ]-dl_codegrp     = me->at_code.
      WHEN 'D_CODEGRP'     . it_nidef[ index ]-d_codegrp      = me->at_code.
      WHEN 'CAUSE_CODEGRP' . it_nicau[ index ]-cause_codegrp  = me->at_code.
      WHEN 'TASK_CODEGRP'  . it_nitas[ index ]-task_codegrp   = me->at_code.
      WHEN 'ACT_CODEGRP'   . it_nifac[ index ]-act_codegrp    = me->at_code.
    ENDCASE.
  ENDMETHOD.

  METHOD set_txt.

    TRY .
        DATA(descricao) = codes1[ cat_typ = me->at_cat_typ code_group = me->at_code_group code = me->at_code ]-shorttxtcd.
      CATCH cx_sy_itab_line_not_found.
        CLEAR descricao.
    ENDTRY.

    CASE field.
      WHEN 'DL_CODE'    . it_niobj[ index ]-txt_objptcd = descricao.
      WHEN 'D_CODE'     . it_nidef[ index ]-txt_probcd  = descricao.
      WHEN 'CAUSE_CODE' . it_nicau[ index ]-txt_causecd = descricao.
      WHEN 'TASK_CODE'  . it_nitas[ index ]-txt_taskcd  = descricao.
      WHEN 'ACT_CODE'   . it_nifac[ index ]-txt_actcd   = descricao.
    ENDCASE.

  ENDMETHOD.

  METHOD set_apontar.
* Apontamento de Atividades.

    DATA: lw_header       TYPE bapi_alm_order_header_e,
          lt_operations   TYPE TABLE OF bapi_alm_order_operation_e,
          lt_return       TYPE TABLE OF bapiret2,
          lt_ordem        TYPE TABLE OF ztpm_d_m_ordem,
          lv_created      TYPE timestampl,
          lv_updated      TYPE timestampl,
          lv_sobra        TYPE string,
          lv_string       TYPE string,
          lt_operacao     TYPE ztpm_d_m_operacao_t,
          lw_ordem        TYPE ztpm_d_m_ordem,
          lt_apontamentos TYPE ztpm_d_m_apont_t,
          lv_line         TYPE bsvx-sttxt.

    it_timetickets =
    VALUE #( FOR ls IN it_opera WHERE ( isdd IS NOT INITIAL AND
                                        isdz IS NOT INITIAL AND
                                        iedd IS NOT INITIAL AND
                                        iedz IS NOT INITIAL )
                                (
                                  orderid         = header-orderid
                                  postg_date      = ls-budat
                                  operation       = ls-activity
                                  sub_oper        = ls-sub_activity
                                  work_cntr       = ls-work_cntr
                                  pers_no         = ls-pernr
                                  exec_start_date = ls-isdd
                                  exec_start_time = ls-isdz
                                  exec_fin_date   = ls-iedd
                                  exec_fin_time   = ls-iedz
                                  fin_conf        = ls-fin_conf
                                  act_work        = ls-afrud
                                  un_work         = ls-duration_normal_unit
                                  dev_reason      = ls-grund
*                                  CLEAR_RES       = LS-CLEAR_RES
                                  act_type        = ls-acttype
                                ) ).

    CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
      IMPORTING
        return        = wa_return
      TABLES
        timetickets   = it_timetickets
        detail_return = tg_return.

    IF NOT line_exists( tg_return[ type = 'E' ] ).

*-US 145467-21-08-2024-#145467-RJF-inicio
      DATA: ls_return_commit  TYPE bapiret2.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = abap_true
        IMPORTING
          return = ls_return_commit.
      IF ls_return_commit-type EQ 'E' OR ls_return_commit-type EQ 'A'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        DATA(lv_err) =  abap_true.
      ENDIF.
*-US 145467-21-08-2024-#145467-RJF-fim

      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number        = header-orderid
        IMPORTING
          es_header     = lw_header
        TABLES
          et_operations = lt_operations
          return        = lt_return.

      IF NOT line_exists( lt_return[ type = 'E' ] ).

        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            objnr            = lw_header-object_no
            spras            = 'P'
          IMPORTING
            line             = lv_line
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc = 0.

          IF lv_line CS 'ABER'.
            lw_ordem-istat = '0'.
          ELSEIF lv_line CS 'LIB'.
            lw_ordem-istat = '1'.
          ELSEIF lv_line CS 'ENTE' OR
                 lv_line CS 'ENCE' .
            lw_ordem-istat = '2'.
          ENDIF.
        ENDIF.

        lw_ordem-aufnr      = lw_header-orderid.
        lw_ordem-auart      = lw_header-order_type.
        lw_ordem-qmnum      = lw_header-notif_no.
        lw_ordem-ktext      = lw_header-short_text.
        lw_ordem-iwerk      = lw_header-planplant.
        lw_ordem-ingpr      = lw_header-plangroup.
        lw_ordem-arbpl      = lw_header-mn_wk_ctr.
        lw_ordem-user4      = lw_header-estimated_costs.
        lw_ordem-ilart      = lw_header-pmacttype.
*lw_ordem-idequipe   =
        lw_ordem-anlzu      = lw_header-systcond.
        lw_ordem-priok      = lw_header-priority.
        lw_ordem-tplnr      = lw_header-funct_loc.
        lw_ordem-equnr      = lw_header-equipment.
        lw_ordem-gstrp      = lw_header-start_date.
        lw_ordem-gltrp      = lw_header-finish_date.
        lw_ordem-erdat      = lw_header-enter_date.

*        SORT it_timetickets BY operation.

        LOOP AT lt_operations ASSIGNING FIELD-SYMBOL(<fs_operation>).
          APPEND INITIAL LINE TO lt_operacao ASSIGNING FIELD-SYMBOL(<fs_operacao>).

          <fs_operacao>-vornr = <fs_operation>-activity.
          <fs_operacao>-ltxa1 = <fs_operation>-description.
          <fs_operacao>-aufnr = lw_header-orderid.
          <fs_operacao>-werks = <fs_operation>-plant.
          <fs_operacao>-arbpl = <fs_operation>-work_cntr.
          <fs_operacao>-steus = <fs_operation>-control_key.
          <fs_operacao>-arbei = <fs_operation>-work_activity.
          <fs_operacao>-arbeh = <fs_operation>-un_work.
          <fs_operacao>-anzzl = <fs_operation>-number_of_capacities.
          <fs_operacao>-pernr = <fs_operation>-pers_no.
          <fs_operacao>-einsa = <fs_operation>-constraint_type_start.
          <fs_operacao>-einse = <fs_operation>-constraint_type_finish.
          <fs_operacao>-ntanf = <fs_operation>-start_cons.
          <fs_operacao>-ntend = <fs_operation>-fin_constr.

          READ TABLE it_timetickets ASSIGNING FIELD-SYMBOL(<fs_time>)
          WITH KEY operation = <fs_operacao>-vornr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            LOOP AT it_timetickets ASSIGNING <fs_time> FROM sy-tabix.

              DATA(lv_tabix_apont) = sy-tabix.

              IF <fs_operacao>-vornr <> <fs_time>-operation.
                EXIT.
              ENDIF.

              APPEND INITIAL LINE TO lt_apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).

              DELETE tg_return WHERE conf_cnt IS INITIAL.

*              READ TABLE tg_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1. "del    FF - 10.10.2023
              READ TABLE tg_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX lv_tabix_apont. "ins    FF - 10.10.2023

              <fs_apontamentos>-rueck       = <fs_return>-conf_cnt.
              <fs_apontamentos>-rmzhl       = <fs_return>-conf_no.
              <fs_apontamentos>-werks       = <fs_time>-plant.
              <fs_apontamentos>-pernr       = <fs_time>-pers_no.
              <fs_apontamentos>-arbpl       = <fs_time>-work_cntr.
              <fs_apontamentos>-vornr       = <fs_time>-operation.
              <fs_apontamentos>-budat       = sy-datum.
              <fs_apontamentos>-isdd        = <fs_time>-exec_start_date.
              <fs_apontamentos>-isdz        = <fs_time>-exec_start_time.
              <fs_apontamentos>-iedd        = <fs_time>-exec_fin_date.
              <fs_apontamentos>-iedz        = <fs_time>-exec_fin_time.
              <fs_apontamentos>-ismnw       = <fs_time>-act_work.
              <fs_apontamentos>-ismne       = <fs_time>-un_work.
              <fs_apontamentos>-aueru       = <fs_time>-fin_conf.

              CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
                EXPORTING
                  i_date = sy-datum    " Data
                  i_time = sy-uzeit  " Hora
                IMPORTING
                  e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

              lv_string = lv_created.
              REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
              CONDENSE lv_string NO-GAPS.

              <fs_apontamentos>-created_at = lv_string(13).


            ENDLOOP.

            <fs_operacao>-apontamentos = lt_apontamentos.
            CLEAR lt_apontamentos[]. "ins    FF - 10.10.2023
          ENDIF.

        ENDLOOP.

        IF lt_operacao IS NOT INITIAL.
          lw_ordem-operacao = lt_operacao.
        ENDIF.

        IF lw_ordem-erdat IS NOT INITIAL AND lw_ordem-erfzeit IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = lw_ordem-erdat    " Data
              i_time = lw_ordem-erfzeit  " Hora
            IMPORTING
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_ordem-created_at = lv_string(13).

        ENDIF.

        IF lw_ordem-aedat IS NOT INITIAL AND lw_ordem-aezeit IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = lw_ordem-aedat    " Data
              i_time = lw_ordem-aezeit  " Hora
            IMPORTING
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string  = lv_updated.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_ordem-updated_at = lv_string(13).

        ENDIF.

****-US 146832-02-08-2024-#146832-RJF-inicio
**** Bug Solto 146832 - Favor não apagar o trecho comentado
**** Conforme alinhado, vamos comentar a parte que faz envio para o mobman
**** até que o endpoint do lado da api seja concluido para solucionar o problema.
***        TRY.
***
***            zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
***                  )->set_dados_ordem( i_data = lw_ordem
***                  )->post_cria_modifica_ordem_app( EXPORTING i_ordem = lw_ordem ).
***
***          CATCH zcx_integracao INTO DATA(ex_integra).    "
***            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
***
***          CATCH zcx_error INTO DATA(ex_error).    "  "
***            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
***
***        ENDTRY.
****-US 146832-02-08-2024-#146832-RJF-fim

      ENDIF.

**  Begin of   "FF #96115
      IF NOT line_exists( lt_return[ type = 'E' ] ).

        IF it_opera[] IS NOT INITIAL.
          SELECT rueck, rmzhl, aufnr, vornr, pernr,
                 isdd, isdz, iedz
            INTO TABLE @DATA(lt_afru)
            FROM afru FOR ALL ENTRIES IN @it_opera
            WHERE aufnr = @it_opera-aufnr
              AND vornr = @it_opera-activity.
          IF sy-subrc <> 0.
            CLEAR lt_afru[].
          ENDIF.
        ENDIF.

        LOOP AT it_opera INTO DATA(wa_opera) WHERE mncod IS NOT INITIAL.

          READ TABLE lt_operations INTO DATA(wa_operations) WITH KEY activity = wa_opera-activity.
          IF sy-subrc <> 0.
            CLEAR wa_operations.
          ENDIF.

          READ TABLE lt_afru WITH KEY aufnr = wa_opera-aufnr
                                      vornr = wa_opera-activity
                                      pernr = wa_opera-pernr
                                      isdd  = wa_opera-isdd
                                      isdz  = wa_opera-isdz
                                      iedz  = wa_opera-iedz
                                      INTO DATA(wa_afru).
          IF sy-subrc = 0.

            UPDATE afru SET zzqmnum   = wa_operations-notif_no
                            zzltxa1   = wa_operations-description
                            zzmncod   = wa_opera-mncod
                            zztxtcdma = wa_opera-txtcdma
                        WHERE rueck = wa_afru-rueck
                          AND rmzhl = wa_afru-rmzhl.

            COMMIT WORK AND WAIT.

          ENDIF.

        ENDLOOP.
      ENDIF.

*  Apontamento das Ações
**  Begin of    #96115  FF
      SORT it_nifac BY act_key DESCENDING.
      READ TABLE it_nifac INDEX 1 INTO DATA(wa_nifac).
      DATA(lv_ultimo_act_key) = wa_nifac-act_key + 1.

*-US 145467-22-07-2024-#145467-RJF-inicio
      SELECT MAX( manum )
        UP TO 1 ROWS
        INTO @DATA(lv_manum)
        FROM qmma
        WHERE qmnum EQ @v_nota.
      IF sy-subrc IS INITIAL.
        lv_ultimo_act_key = lv_manum + 1.
      ENDIF.
*-US 145467-22-07-2024-#145467-RJF-fim

      IF lv_ultimo_act_key IS INITIAL.
        lv_ultimo_act_key = '0001'.
      ENDIF.

      CLEAR: it_nifac[], it_nifac.

*      EXPORT it_aponta_export FROM it_aponta[] TO MEMORY ID 'ZPM0102'. "Export para ser importado no include ZPM_DISPARA_API_NOTA_MOB "" RJF Rollback PRD - Bug Solto 146832

      LOOP AT it_aponta INTO DATA(wa_aponta).
        DATA(lv_tabix) = sy-tabix.

        READ TABLE lt_operations INTO DATA(wa_oper) WITH KEY activity = wa_aponta-activity.
        IF sy-subrc = 0 AND wa_oper-notif_no IS NOT INITIAL.

          APPEND INITIAL LINE TO it_nifac ASSIGNING FIELD-SYMBOL(<fs_notactve>).

          IF <fs_notactve> IS ASSIGNED.
            MOVE:
            'X' TO <fs_notactve>-check,
            wa_aponta-mncod    TO <fs_notactve>-act_code,
            wa_aponta-mngrp    TO <fs_notactve>-act_codegrp,
            wa_aponta-txtcdma  TO <fs_notactve>-txt_actcd,
            wa_aponta-isdd     TO <fs_notactve>-start_date,
            wa_aponta-isdz     TO <fs_notactve>-start_time,
            wa_aponta-iedd     TO <fs_notactve>-end_date,
            wa_aponta-iedz     TO <fs_notactve>-end_time.

            IF lv_tabix = 1.
              <fs_notactve>-act_key = lv_ultimo_act_key.
            ELSE.
              lv_ultimo_act_key = lv_ultimo_act_key + 1.
              <fs_notactve>-act_key = lv_ultimo_act_key.
            ENDIF.

            tnotifactv   = VALUE #( FOR l3 IN it_nifac WHERE ( act_key IS NOT INITIAL )
                                    (
                                      act_key      = l3-act_key
                                      act_sort_no  = l3-act_key"ACT_SORT_NO
                                      acttext      = l3-acttext
                                      act_codegrp  = l3-act_codegrp
                                      act_code     = l3-act_code
                                      start_date   = l3-start_date
                                      start_time   = l3-start_time
                                      end_date     = l3-end_date
                                      end_time     = l3-end_time

                                     ) ).

*-US 145467-22-07-2024-#145467-RJF-inicio
*            CLEAR it_return_add[].
*            CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
*              EXPORTING
*                number    = wa_oper-notif_no
*              TABLES
*                notifactv = tnotifactv
*                return    = it_return_add.
*
*            APPEND LINES OF it_return_add    TO t_return.
*
*            IF NOT line_exists( it_return_add[ type = 'E' ] ).
*              me->save_commit( ).
*            ENDIF.
*-US 145467-22-07-2024-#145467-RJF-fim
          ENDIF.
        ENDIF.
      ENDLOOP.

*-US 145467-22-07-2024-#145467-RJF-inicio
* Retirarda do loop
      CLEAR it_return_add[].
      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
        EXPORTING
          number    = wa_oper-notif_no
        TABLES
          notifactv = tnotifactv
          return    = it_return_add.

      APPEND LINES OF it_return_add    TO t_return.

      IF NOT line_exists( it_return_add[ type = 'E' ] ).
        me->save_commit( ).
      ENDIF.
* Retirarda do loop
*-US 145467-22-07-2024-#145467-RJF-fim

*      CLEAR it_return_add[].
*      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
*        EXPORTING
*          number    = wa_oper-notif_no
*        TABLES
*          notifactv = tnotifactv
*          return    = it_return_add.
*
*      APPEND LINES OF it_return_add    TO t_return.
*
*      IF NOT line_exists( it_return_add[ type = 'E' ] ).
*        me->save_commit( ).
*      ENDIF.
** End of FF

****      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
****        EXPORTING
****          wait = abap_true.
****      FREE it_opera.
****      FREE it_aponta.

      IF v_conf_enc IS NOT INITIAL.
        obj_main->set_encerra_ordem( ).
      ENDIF.
    ENDIF.

    it_return = VALUE #( FOR lr IN tg_return
                                            (
                                              type        = lr-type
                                              id          = lr-message_id
                                              number      = lr-message_number
                                              message     = lr-message
                                              log_no      = lr-log_number
                                              log_msg_no  = lr-log_msg_no
                                              message_v1  = lr-message_v1
                                              message_v2  = lr-message_v2
                                              message_v3  = lr-message_v3
                                              message_v4  = lr-message_v4
                                              parameter   = lr-parameter
                                              row         = lr-row
                                              field       = lr-field
                                              system      = lr-system
                                            )
                       ).

    IF wa_return IS NOT INITIAL.
      APPEND wa_return TO it_return.
    ENDIF.
    APPEND LINES OF it_return TO t_return.
    FREE it_return.

  ENDMETHOD.

  METHOD set_encerra_ordem.

    PERFORM check_req_and_ped_pendente USING header-orderid.

    IF t_req_pend IS NOT INITIAL OR
       t_ped_pend IS NOT INITIAL.
      MESSAGE i007 WITH header-orderid DISPLAY LIKE 'E'.
    ELSE.


      DATA: equipment  TYPE equnr,
            standorder TYPE daufn,
            settlorder TYPE ilom_ordst.

      CLEAR: it_methods, wa_return.

      DATA: lv_refnum  TYPE ifrefnum.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = header-orderid.
      APPEND wa_methods TO it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      APPEND wa_methods TO it_methods.

      wa_header-orderid = header-orderid.
      APPEND wa_header TO it_header.


      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      APPEND LINES OF it_return TO t_return.
      IF NOT line_exists( it_return[ type = 'E' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = abap_true
          IMPORTING
            return = wa_return.
      ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        WAIT   = 'X'
*      IMPORTING
*        RETURN = WA_RETURN.

      me->set_get_return( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_encerra_ordem_nota.
    DATA: wa_return TYPE bapiret2.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
      EXPORTING
        number       = ld_number-notif_no
        syststat     = ld_syststat
        testrun      = ld_testrun
      IMPORTING
        systemstatus = ld_systemstatus-systatus
        userstatus   = ld_systemstatus-usrstatus
      TABLES
        return       = it_return.  "  BAPI_ALM_NOTIF_CLOSE

    IF it_return IS INITIAL.
      MESSAGE s408(im) WITH ld_number-notif_no.

      APPEND LINES OF it_return TO t_return.
      me->set_get_return( ).

      me->save_commit( ).
    ELSE.
      APPEND LINES OF it_return TO t_return.
      me->set_get_return( ).

*      LOOP AT IT_RETURN INTO WA_RETURN.
*        MESSAGE E000(O0) WITH WA_RETURN-MESSAGE.
*      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD set_edit_nota.
    me->refresh( ).
  ENDMETHOD.
***-US 146832-02-08-2024-#146832-RJF-início
*** Bug Solto 146832 - Não apagar o trecho
*** Conforme alinhado, vamos comentar a parte que faz envio para o mobman
*** até que o endpoint do lado da api seja concluido para solucionar o problema.
  METHOD set_avaria. "-US 145467-22-07-2024-#145467-RJF-Início

    CONSTANTS:
          lc_change_modify VALUE 'M'.
    DATA: ls_notifheader_c TYPE  bapi2080_nothdri,
          ls_notifheader_x TYPE  bapi2080_nothdri_x,
          ls_nothdre       TYPE  bapi2080_nothdre,
          lv_xauszt        TYPE  viqmel-auszt,
          lv_eauszt        TYPE  riwo00-eauszt,
          lv_auszt         TYPE  auszt,
          lv_ini           TYPE  ausvn, "ini
          lv_fim           TYPE  ausbs, "fim
          lv_inih          TYPE  auztv, "inih
          lv_fimh          TYPE  auztb.
    CHECK it_aponta[] IS NOT INITIAL.

    SELECT * "ausbs, ausvn " fim e ini
      UP TO 1 ROWS
    INTO @wa_viqmel
    FROM viqmel
    WHERE qmnum EQ @v_nota.
    ENDSELECT.

    DATA(it_apontai) = it_aponta.
    DATA(it_apontaf) = it_aponta.

    SORT it_apontai BY isdd isdz ASCENDING.
    SORT it_apontaf BY iedd iedz DESCENDING.

    IF wa_viqmel-msaus IS INITIAL.
*    IF i_grava IS INITIAL AND i_ini IS INITIAL AND i_fim IS INITIAL.
      READ TABLE it_apontai INTO DATA(wa_apontai) INDEX 1.
      IF sy-subrc IS INITIAL.
        IF wa_viqmel-ausvn GT wa_apontai-isdd.
          ls_notifheader_c-strmlfndate =  wa_apontai-isdd.
          ls_notifheader_c-strmlfntime =  wa_apontai-isdz.
          ls_notifheader_x-strmlfndate =  abap_on.
          ls_notifheader_x-strmlfntime =  abap_on.
        ELSEIF wa_viqmel-ausvn EQ wa_apontai-isdd AND wa_viqmel-auztv GT wa_apontai-isdz.
          ls_notifheader_c-strmlfndate =  wa_apontai-isdd.
          ls_notifheader_c-strmlfntime =  wa_apontai-isdz.
          ls_notifheader_x-strmlfndate =  abap_on.
          ls_notifheader_x-strmlfntime =  abap_on.
        ENDIF.
      ENDIF.

      READ TABLE it_apontaf INTO DATA(wa_apontaf) INDEX 1.
      IF sy-subrc IS INITIAL.
        IF wa_viqmel-ausbs LT wa_apontaf-iedd.
          ls_notifheader_c-endmlfndate =  wa_apontaf-iedd.
          ls_notifheader_c-endmlfntime =  wa_apontaf-iedz.
          ls_notifheader_x-endmlfndate =  abap_on.
          ls_notifheader_x-endmlfntime =  abap_on.
        ELSEIF wa_viqmel-ausbs EQ wa_apontaf-iedd AND wa_viqmel-auztb LT wa_apontaf-iedz.
          ls_notifheader_c-endmlfndate =  wa_apontaf-iedd.
          ls_notifheader_c-endmlfntime =  wa_apontaf-iedz.
          ls_notifheader_x-endmlfndate =  abap_on.
          ls_notifheader_x-endmlfntime =  abap_on.
        ENDIF.
      ENDIF.
    ENDIF.
*    IF i_ini IS NOT INITIAL.
*      READ TABLE it_apontai INTO wa_apontai INDEX 1. "
*      IF sy-subrc IS INITIAL.
*        ls_notifheader_c-strmlfndate =  wa_apontai-isdd.
*        ls_notifheader_c-strmlfntime =  wa_apontai-isdz.
*        ls_notifheader_x-strmlfndate =  abap_on.
*        ls_notifheader_x-strmlfntime =  abap_on.
*      ENDIF.
*    ENDIF.

    IF wa_viqmel-msaus IS NOT INITIAL.
      READ TABLE it_apontaf INTO wa_apontaf INDEX 1.
      IF sy-subrc IS INITIAL.
        IF wa_viqmel-ausbs LT wa_apontaf-iedd.
          ls_notifheader_c-endmlfndate =  wa_apontaf-iedd.
          ls_notifheader_c-endmlfntime =  wa_apontaf-iedz.
          ls_notifheader_x-endmlfndate =  abap_on.
          ls_notifheader_x-endmlfntime =  abap_on.
        ELSEIF wa_viqmel-ausbs EQ wa_apontaf-iedd AND wa_viqmel-auztb LT wa_apontaf-iedz.
          ls_notifheader_c-endmlfndate =  wa_apontaf-iedd.
          ls_notifheader_c-endmlfntime =  wa_apontaf-iedz.
          ls_notifheader_x-endmlfndate =  abap_on.
          ls_notifheader_x-endmlfntime =  abap_on.
        ENDIF.
      ENDIF.
    ENDIF.
**    ls_notifheader_c-breakdown =  abap_on.
**    ls_notifheader_x-breakdown =  abap_on.
*
*    IF ( ls_notifheader_c-strmlfndate IS INITIAL OR ls_notifheader_c-strmlfndate EQ '00000000' ).
*      lv_ini  = iw_viqmel-ausvn.
*      lv_inih = iw_viqmel-auztv.
*    ELSE.
*      lv_ini  = ls_notifheader_c-strmlfndate.
*      lv_inih = ls_notifheader_c-strmlfntime.
*    ENDIF.
*
*    IF ( ls_notifheader_c-endmlfndate IS INITIAL OR ls_notifheader_c-endmlfndate EQ '00000000' ).
*      lv_fim  = iw_viqmel-ausbs.
*      lv_fimh = iw_viqmel-auztb.
*    ELSE.
*      lv_fim  = ls_notifheader_c-endmlfndate.
*      lv_fimh = ls_notifheader_c-endmlfntime.
*    ENDIF.
*
*    PERFORM dauer_ermitteln_f30 IN PROGRAM sapliqs0 IF FOUND
*                                    USING lv_ini     "ini
*                                          lv_fim     "fim
*                                          lv_inih    "inih
*                                          lv_fimh    "fimh
*                                          lv_xauszt. "Intern
*
*    PERFORM extern_auszt_ermitteln_f30 IN PROGRAM sapliqs0 IF FOUND
*                             USING 'S'       "imaueh'
*                                   lv_xauszt
*                                   iw_viqmel-maueh
*                                   lv_eauszt. " oauszt
*
*    IF lv_eauszt IS NOT INITIAL.
*      lv_auszt = CONV #( lv_eauszt ).
*    ENDIF.

*    CALL FUNCTION 'DFS_PM_NOTIFICATION_CHANGE'
*      EXPORTING
*        iv_notification    = v_nota
*        is_notif_header    = ls_notifheader_c
*        iv_notif_break_dur = lv_auszt.
*
*    IF sy-subrc IS INITIAL.
*
**      COMMIT WORK AND WAIT.
**      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**        EXPORTING
**          wait   = abap_true
**        IMPORTING
**          return = wa_return.
*    ENDIF.

    CALL FUNCTION 'ALM_ME_NOTIFICATION_CHANGE2'
      EXPORTING
        change_mode                = lc_change_modify
        notif_no                   = v_nota
        notification_header        = ls_notifheader_c
        notification_header_x      = ls_notifheader_x
      IMPORTING
        notification_header_export = ls_nothdre.

  ENDMETHOD.         "-US 145467-22-07-2024-#145467-RJF-Fim
  METHOD set_save_nota.

    DATA: lt_result       TYPE  bapiret2_t,
          lt_dados_catalo TYPE TABLE OF  zpme0072.



*    PERFORM f_salva_texto_longo. "Texto longo é passado BAPI BAPI_ALM_NOTIF_DATA_ADD, ->tnotfulltxt

    PERFORM f_guarda_dados_tela TABLES it_dados_tela.

    LOOP AT it_dados_tela INTO zepm_aponta_cat_notas WHERE notifno IS NOT INITIAL.

      v_nota = zepm_aponta_cat_notas-notifno.

      PERFORM f_le_texto_longo_notif USING v_nota abap_true.

      " Preencha as tabelas internas com os dados relevantes
      DATA(lt_novo_texto_longo) = gt_texto[].
      DELETE lt_novo_texto_longo WHERE qmnum <> v_nota.

      LOOP AT lt_novo_texto_longo ASSIGNING FIELD-SYMBOL(<fs_novo_texto_longo>).
        DATA(lv_tabix) = sy-tabix.
        READ TABLE gt_lines
        WITH KEY tdline = <fs_novo_texto_longo>-texto
        TRANSPORTING NO FIELDS..

        IF sy-subrc = 0.
          "Deletando os textos que já existiam na nota.
          DELETE lt_novo_texto_longo INDEX lv_tabix.
        ENDIF.
      ENDLOOP.

      tnotfulltxt = VALUE #(
      FOR ls IN lt_novo_texto_longo (
                        objtype    = 'QMEL'
                        objkey     = ls-qmnum
                        format_col = '*'
                        text_line  = ls-texto ) ).

      APPEND
      VALUE #(  qmnum = zepm_aponta_cat_notas-notifno
                fenum = '0000'
                otkat = 'B'
                otgrp = zepm_aponta_cat_notas-otgrp
                oteil = zepm_aponta_cat_notas-oteil
                fekat = 'C'
                fegrp = zepm_aponta_cat_notas-fegrp
                fecod = zepm_aponta_cat_notas-fecod
                urkat = '5'
                urgrp = zepm_aponta_cat_notas-urgrp
                urcod = zepm_aponta_cat_notas-urcod
                mnkat = ''
                mngrp = zepm_aponta_cat_notas-mngrp
                mncod = zepm_aponta_cat_notas-mncod
                fetxt = zepm_aponta_cat_notas-fetxt
                urstx = ''
      ) TO lt_dados_catalo.

      CALL FUNCTION 'ZPM_ZCHANGE_CATAL_NOTIF'
        EXPORTING
          i_qmnum        = zepm_aponta_cat_notas-notifno
        IMPORTING
          r_result       = lt_result
        TABLES
          t_texto_logo   = tnotfulltxt
          t_dados_catalo = lt_dados_catalo.

      CLEAR: lt_dados_catalo[],
             lt_dados_catalo.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_get_return.

    CHECK t_return  IS NOT INITIAL.

    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = t_return.

    CALL FUNCTION 'Z_GRAVA_LOG_PM'
      TABLES
        t_return = t_return.

**  Begin of    #96115  FF
    READ TABLE t_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CLEAR: it_timetickets[], it_timetickets.
    ENDIF.
** End of FF

    FREE t_return.

  ENDMETHOD.

  METHOD add_line.

    IF REDUCE i( INIT x = 0 FOR wa1 IN it_opera WHERE ( activity IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_opera.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR wa2 IN it_niobj WHERE ( dl_codegrp IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_niobj.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR wa3 IN it_nidef WHERE ( d_codegrp IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_nidef.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR wa4 IN it_nicau WHERE ( cause_codegrp IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_nicau.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR wa5 IN it_nitas WHERE ( task_codegrp IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_nitas.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR wa6 IN it_nifac WHERE ( act_codegrp IS INITIAL ) NEXT x = x + 1 ) IS INITIAL.
      APPEND INITIAL LINE TO it_nifac.
    ENDIF.

    me->set_block( ).

  ENDMETHOD.

  METHOD set_notificacao.

    FREE: tnotifitem, tnotifcaus, tnotifactv, tnotiftask.
    FREE: tnotifitem_x, tnotifcaus_x, tnotifactv_x, tnotiftask_x.

**  Begin of    #96115  FF

    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
      EXPORTING
        number             = v_nota
      IMPORTING
        notifheader_export = gs_notifheader_export
        notifhdtext        = gs_notifhdtext
      TABLES
        notitem            = it_notiteme
        notifcaus          = it_notcause
        notifactv          = it_notactve
        notiftask          = it_nottaske.

    DELETE it_notiteme WHERE delete_flag IS NOT INITIAL.
    DELETE it_notcause WHERE delete_flag IS NOT INITIAL.
    DELETE it_notactve WHERE delete_flag IS NOT INITIAL.
    DELETE it_nottaske WHERE delete_flag IS NOT INITIAL.


    IF it_notiteme[] IS NOT INITIAL.
*      READ TABLE it_notiteme INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_item>).
*      IF sy-subrc = 0."Caso o valor não tem sofriado alteração na tela, deleto ele não dar erro na BAPI.
*
*        READ TABLE it_dados_tela with KEY notifno = <fs_item>-notif_no INTO data(wa_dados_tela).
*
*        IF wa_dados_tela-otgrp = <fs_item>-dl_codegrp.
*          CLEAR zepm_aponta_cat_notas-otgrp.
*        ENDIF.
*
*        IF wa_dados_tela-oteil  = <fs_item>-dl_code.
*          CLEAR zepm_aponta_cat_notas-oteil.
*        ENDIF.
*
*        IF wa_dados_tela-txtcdot = <fs_item>-txt_objptcd.
*          CLEAR zepm_aponta_cat_notas-txtcdot.
*        ENDIF.
*
*        IF wa_dados_tela-fetxt = <fs_item>-descript.
*          CLEAR zepm_aponta_cat_notas-fetxt.
*        ENDIF.
*
*      ENDIF.

      MOVE-CORRESPONDING it_notiteme TO it_niobj[].
      READ TABLE it_niobj INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_notitem>).

    ELSE.
      IF zepm_aponta_cat_notas-otgrp IS NOT INITIAL.
        APPEND INITIAL LINE TO it_niobj ASSIGNING <fs_notitem>.
      ENDIF.
    ENDIF.
    IF <fs_notitem> IS ASSIGNED.

      IF <fs_notitem>-item_key IS INITIAL.
        <fs_notitem>-item_key = '0001'.
        <fs_notitem>-item_sort_no = '0001'.
      ENDIF.

      MOVE:
*      '0001' TO <fs_notitem>-item_key,
*      '0001' TO <fs_notitem>-item_sort_no,
      'X' TO <fs_notitem>-check,
      zepm_aponta_cat_notas-otgrp   TO <fs_notitem>-dl_codegrp,
      zepm_aponta_cat_notas-oteil   TO <fs_notitem>-dl_code,
      zepm_aponta_cat_notas-txtcdot TO <fs_notitem>-txt_objptcd,
      zepm_aponta_cat_notas-fetxt   TO <fs_notitem>-descript.
    ENDIF.

    IF it_notcause[] IS NOT INITIAL.
      MOVE-CORRESPONDING it_notcause TO it_nicau.
      READ TABLE it_nicau INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_notcause>).
    ELSE.
      IF zepm_aponta_cat_notas-urgrp IS NOT INITIAL.
        APPEND INITIAL LINE TO it_nicau ASSIGNING <fs_notcause>.
      ENDIF.
    ENDIF.
    IF <fs_notcause> IS ASSIGNED.
      IF <fs_notcause>-item_key IS INITIAL.
        <fs_notcause>-item_key = '0001'.
      ENDIF.

      MOVE:
*      '0001' TO <fs_notcause>-item_key,
      '0001' TO <fs_notcause>-item_sort_no,
*      '0001' TO <fs_notcause>-cause_key,
*      '0001' TO <fs_notcause>-cause_sort_no,
      'X' TO <fs_notcause>-check,
      zepm_aponta_cat_notas-urgrp   TO <fs_notcause>-cause_codegrp,
      zepm_aponta_cat_notas-urcod   TO <fs_notcause>-cause_code,
      zepm_aponta_cat_notas-txtcdur TO <fs_notcause>-txt_causecd,
      zepm_aponta_cat_notas-urtxt   TO <fs_notcause>-causetext.
    ENDIF.

    IF it_notactve[] IS NOT INITIAL.
      MOVE-CORRESPONDING it_notactve[] TO it_nifac[].
      READ TABLE it_nifac INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_notactve>).
    ELSE.
      IF zepm_aponta_cat_notas-mngrp IS NOT INITIAL.
        APPEND INITIAL LINE TO it_nifac ASSIGNING <fs_notactve>.
      ENDIF.
    ENDIF.

    tnotifitem   = VALUE #( FOR l1 IN it_niobj WHERE ( check EQ input AND item_key IS NOT INITIAL )
                            (
                              item_key     = l1-item_key
                              item_sort_no = l1-item_key"COND #( WHEN INPUT EQ ABAP_TRUE THEN L1-ITEM_SORT_NO ELSE L1-ITEM_KEY )
                              descript     = l1-descript
                              dl_codegrp   = l1-dl_codegrp
                              dl_code      = l1-dl_code
                            ) ).

    READ TABLE tnotifitem ASSIGNING FIELD-SYMBOL(<item>) INDEX 1.
    IF <item> IS ASSIGNED.
      <item>-d_codegrp = zepm_aponta_cat_notas-fegrp.
      <item>-d_code    = zepm_aponta_cat_notas-fecod.
    ENDIF.

    tnotifitem_x = VALUE #( FOR l1 IN it_niobj WHERE ( check EQ input AND item_key IS NOT INITIAL )
                            (
                              item_key     = l1-item_key
                              item_sort_no = abap_true
                              descript     = abap_true
                              d_codegrp    = abap_true
                              d_code       = abap_true
                              dl_codegrp   = abap_true
                              dl_code      = abap_true
                            ) ).

    tnotifcaus   = VALUE #( FOR l2 IN it_nicau WHERE ( check EQ input AND item_key IS NOT INITIAL )
                            (
                              cause_key       = l2-cause_key"'0001'
                              cause_sort_no   = l2-item_key"'0001'
                              item_key        = l2-item_key
                              causetext       = l2-causetext
                              cause_codegrp   = l2-cause_codegrp
                              cause_code      = l2-cause_code
                              item_sort_no    = l2-item_key
                            ) ).

    tnotifcaus_x = VALUE #( FOR l2 IN it_nicau WHERE ( check EQ input AND item_key IS NOT INITIAL )
                            (
                              cause_key       = l2-cause_key
                              cause_sort_no   = abap_true
                              item_key        = l2-item_key
                              causetext       = abap_true
                              cause_codegrp   = abap_true
                              cause_code      = abap_true
                              item_sort_no    = l2-item_key
                             ) ).

*    tnotifactv   = VALUE #( FOR l3 IN it_nifac WHERE ( check EQ input AND act_key IS NOT INITIAL )
*                            (
*                              act_key      = l3-act_key
*                              act_sort_no  = l3-act_key"ACT_SORT_NO
*                              acttext      = l3-acttext
*                              act_codegrp  = l3-act_codegrp
*                              act_code     = l3-act_code
*                             ) ).
*
*    tnotifactv_x = VALUE #( FOR l3 IN it_nifac WHERE ( check EQ input AND act_key IS NOT INITIAL )
*                             (
*                              act_key      = l3-act_key
*                              act_sort_no  = abap_true
*                              acttext      = abap_true
*                              act_codegrp  = abap_true
*                              act_code     = abap_true
*                             ) ).

    tnotiftask   = VALUE #( FOR l4 IN it_nitas WHERE ( check EQ input AND task_key IS NOT INITIAL )
                            (
                              task_key      = l4-task_key
                              task_sort_no  = l4-task_key"TASK_SORT_NO
                              task_text     = l4-task_text
                              task_codegrp  = l4-task_codegrp
                              task_code     = l4-task_code
                            ) ).

    tnotiftask_x = VALUE #( FOR l4 IN it_nitas WHERE ( check EQ input AND task_key IS NOT INITIAL )
                            (
                              task_key      = l4-task_key
                              task_sort_no  = abap_true
                              task_text     = abap_true
                              task_codegrp  = abap_true
                              task_code     = abap_true
                            ) ).


  ENDMETHOD.

  METHOD save_commit.

    DATA ls_return_commit  TYPE bapiret2.

    CALL FUNCTION 'ALM_PM_NOTIFICATION_SAVE'
      EXPORTING
        number = v_nota "ld_number-notif_no
      TABLES
        return = it_return.

    APPEND LINES OF it_return TO t_return.

    IF NOT line_exists( it_return[ type = 'E' ] ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = abap_true
        IMPORTING
          return = ls_return_commit.

*-US 145467-21-08-2024-#145467-RJF-fim
      IF ls_return_commit-type EQ 'E' OR ls_return_commit-type EQ 'A'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        DATA(lv_err) =  abap_true.
      ENDIF.
*-US 145467-21-08-2024-#145467-RJF-fim

    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CHECK v_ucomm <> 'BTN_CANCEL'. "#96115  FF  07.03.2023

  obj_main->screen( ).
