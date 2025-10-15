*&---------------------------------------------------------------------*
*&  Include           ZMMR182_F01
*&---------------------------------------------------------------------*
CLASS lc_alv DEFINITION.
  PUBLIC SECTION.
*
*   Event Handler for HOTSPOT event
    METHODS:
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

    METHODS:
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

    METHODS: on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

ENDCLASS.

CLASS lc_alv IMPLEMENTATION.

  METHOD on_link_click.
*
*   Get the Sales Order number from the table
    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX row.
    IF sy-subrc IS INITIAL.
      CASE column.
        WHEN 'EBELN'.
          IF <fs_saida>-ebeln IS NOT INITIAL.
            SET PARAMETER ID 'BES' FIELD <fs_saida>-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'MIGO'.
          IF <fs_saida>-migo IS NOT INITIAL.
            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ''
                i_skip_first_screen = 'X'
                i_deadend           = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = <fs_saida>-migo
                i_mjahr             = <fs_saida>-anomigo
              EXCEPTIONS
                illegal_combination = 1
                OTHERS              = 2.

          ENDIF.

        WHEN 'MIRO'.

          IF <fs_saida>-miro IS NOT INITIAL.

            SET PARAMETER ID 'RBN' FIELD <fs_saida>-miro.
            SET PARAMETER ID 'GJR' FIELD <fs_saida>-anomiro.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'FOLHA'.

          IF <fs_saida>-folha IS NOT INITIAL.

            SET PARAMETER ID 'LBL' FIELD <fs_saida>-folha.
            CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN OTHERS.

      ENDCASE.

    ENDIF.
*
  ENDMETHOD.


  METHOD on_user_command.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    DATA: lt_rows       TYPE salv_t_row,
          lv_docestorno TYPE bapi_incinv_fld-inv_doc_no,
          lt_return     TYPE TABLE OF bapiret2.

    CASE e_salv_function.

      WHEN 'ESTORMIGO'.

        lo_selections = gr_alv->get_selections( ).

        lt_rows = lo_selections->get_selected_rows( ).

        LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

          READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_row>.
          IF sy-subrc IS INITIAL.
            IF <fs_saida>-folha IS NOT INITIAL.
              PERFORM f_estorno_folha USING <fs_saida>-folha.
            ELSE.
              IF <fs_saida>-migo IS INITIAL.
                MESSAGE 'Registro sem folha de aceite ou MIGO!' TYPE 'S' DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.

            ENDIF.

            IF <fs_saida>-folha IS INITIAL.
              IF <fs_saida>-migo IS NOT INITIAL.
                PERFORM f_estorno_migo USING <fs_saida>-migo
                                             <fs_saida>-anomigo.
              ELSE.
                MESSAGE 'Registro não possui Migo!' TYPE 'S' DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.

      WHEN 'ESTORMIRO'.

        lo_selections = gr_alv->get_selections( ).

        lt_rows = lo_selections->get_selected_rows( ).

        LOOP AT lt_rows ASSIGNING <fs_row>.

          READ TABLE t_saida ASSIGNING <fs_saida> INDEX <fs_row>.
          IF sy-subrc IS INITIAL.

            IF <fs_saida>-miro IS INITIAL.
              MESSAGE 'Registro não possui Miro!' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

            CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
              EXPORTING
                invoicedocnumber          = <fs_saida>-miro
                fiscalyear                = <fs_saida>-anomiro
                reasonreversal            = '02'
                postingdate               = sy-datum
              IMPORTING
                invoicedocnumber_reversal = lv_docestorno
              TABLES
                return                    = lt_return.
            IF lv_docestorno IS NOT INITIAL.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

              MESSAGE 'Miro ' && space && <fs_saida>-miro && ' estornado com sucesso!' TYPE 'I'.

            ELSE.

              SORT lt_return BY type.
              READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
              IF sy-subrc IS INITIAL.
                MESSAGE ID <fs_return>-id TYPE 'I' NUMBER <fs_return>-number WITH <fs_return>-message_v1.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

    ENDCASE.

  ENDMETHOD.

  METHOD handle_toolbar.

    DATA: ls_toolbar  TYPE stb_button.

* append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.


    CLEAR ls_toolbar.
    MOVE 'ESTORMIGO' TO ls_toolbar-function.
    MOVE icon_employee TO ls_toolbar-icon.
    MOVE 'Estorno Migo/Folha' TO ls_toolbar-quickinfo.
    MOVE 'Estorno Migo/Folha' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.


    CLEAR ls_toolbar.
    MOVE 'ESTORMIRO' TO ls_toolbar-function.
    MOVE icon_employee TO ls_toolbar-icon.
    MOVE 'Estorno Miro' TO ls_toolbar-quickinfo.
    MOVE 'Estorno Miro' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
ENDCLASS.

FORM f_select_data2 .

  DATA: lv_index         TYPE sy-tabix,
        lv_soma_qtd      TYPE ekbe-menge,
        lv_soma_vlr      TYPE ekbe-dmbtr,
        lr_tipos_pedidos TYPE RANGE OF bsart.

*  SELECT *
*    FROM tvarvc
*    INTO TABLE @DATA(lt_tvarv)
*    WHERE name = 'ZMM_TIPOS_PEDIDO_NAO_TRANSITOR'.
*  IF sy-subrc IS INITIAL.
*    LOOP AT lt_tvarv ASSIGNING FIELD-SYMBOL(<fs_tvarv>).
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarv>-low ) TO lr_tipos_pedidos.
*    ENDLOOP.
*  ENDIF.


*** Seleções MIGO
  SELECT be~ebeln
         be~ebelp
         be~zekkn
         be~belnr
         be~lfbnr
         be~budat
         be~menge
         be~werks
         be~gjahr
         be~dmbtr
         ko~bukrs
         ko~bsart
         be~bwart
         be~vgabe
         be~matnr
         be~shkzg
    FROM ekbe AS be
    INNER JOIN ekko AS ko
    ON ko~ebeln = be~ebeln
    INNER JOIN ekpo AS po
    ON po~ebeln = be~ebeln
   AND po~ebelp = be~ebelp
    INTO TABLE t_ekbe
    WHERE be~ebeln IN s_ebeln
      AND ko~bukrs IN s_bukrs
      AND po~webre = abap_true
      AND po~werks IN s_werks
      AND be~budat IN s_budat
      AND be~vgabe IN ( '1','2' )
      AND ko~bsart IN lr_tipos_pedidos.


  LOOP AT t_ekbe  ASSIGNING FIELD-SYMBOL(<fs_ekbe_aux>).
    CONCATENATE <fs_ekbe_aux>-belnr <fs_ekbe_aux>-gjahr INTO <fs_ekbe_aux>-awkey.
  ENDLOOP.

  LOOP AT t_ekbe INTO DATA(w_ekbe).
    COLLECT w_ekbe INTO t_ekbe_aux.
  ENDLOOP.

  t_ekbe[] = t_ekbe_aux[].
  REFRESH t_ekbe_aux.

  IF t_ekbe[] IS NOT INITIAL.
    SELECT ebeln ebelp lfbnr belnr gjahr vgabe dmbtr
      FROM ekbe
      INTO TABLE t_ekbe_folha
      FOR ALL ENTRIES IN t_ekbe
      WHERE ebeln = t_ekbe-ebeln
      AND ebelp   = t_ekbe-ebelp
      AND vgabe   = '9'.
  ENDIF.


  SELECT *
    FROM bkpf AS b
    INTO TABLE t_bkpf
    FOR ALL ENTRIES IN t_ekbe
    WHERE awkey = t_ekbe-awkey.

  SELECT *
    FROM bseg AS b
    INTO TABLE t_bseg
    FOR ALL ENTRIES IN t_bkpf
    WHERE bukrs = t_bkpf-bukrs
    AND   belnr = t_bkpf-belnr
    AND   gjahr = t_bkpf-gjahr
    AND   hkont = '0000212100'
    AND   ebeln IN s_ebeln.

  SORT t_ekbe      BY ebeln ebelp belnr gjahr vgabe.
  SORT t_bkpf      BY bukrs belnr gjahr.

  LOOP AT t_bseg INTO DATA(w_bseg).
    CLEAR w_bseg_tot.
    MOVE-CORRESPONDING w_bseg TO w_bseg_tot.

    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = w_bseg-bukrs
                                                 belnr = w_bseg-belnr
                                                 gjahr = w_bseg-gjahr BINARY SEARCH.

    READ TABLE t_ekbe INTO w_ekbe WITH KEY ebeln = w_bseg-ebeln
                                           ebelp = w_bseg-ebelp
                                           belnr = w_bkpf-awkey+0(10)
                                           gjahr = w_bseg-awkey+10(4)
                                           vgabe = '2' BINARY SEARCH.
    IF sy-subrc = 0. " MIRO
      IF w_bseg-shkzg = 'H'.
        MULTIPLY w_bseg_tot-dmbtr BY -1.
        MULTIPLY w_bseg_tot-menge BY -1.
      ENDIF.
      w_bseg_tot-dmbtr2 = w_bseg_tot-dmbtr.
      w_bseg_tot-menge2 = w_bseg_tot-menge.
      CLEAR: w_bseg_tot-dmbtr, w_bseg_tot-menge.
    ELSE.
      IF w_bseg-shkzg = 'S'.
        MULTIPLY w_bseg_tot-dmbtr BY -1.
        MULTIPLY w_bseg_tot-menge BY -1.
      ENDIF.
    ENDIF.
    CLEAR:
    w_bseg_tot-belnr,
    w_bseg_tot-gjahr,
    w_bseg_tot-budat,
    w_bseg_tot-zekkn,
    w_bseg_tot-lfbnr,
    w_bseg_tot-bsart,
    w_bseg_tot-bwart,
    w_bseg_tot-vgabe,
    w_bseg_tot-matnr,
    w_bseg_tot-shkzg,
    w_bseg_tot-awkey.
    COLLECT w_bseg_tot INTO t_bseg_tot .

  ENDLOOP.
ENDFORM.

FORM f_process_data2 .

  DATA: lv_index TYPE sy-tabix.
  SORT t_ekbe      BY ebeln ebelp vgabe belnr DESCENDING gjahr DESCENDING.
  SORT t_ekbe_folha BY ebeln ebelp belnr DESCENDING gjahr DESCENDING.
  SORT t_bseg_tot BY ebeln ebelp.
  SORT t_ekbe_folha BY belnr vgabe.
  LOOP AT t_bseg_tot ASSIGNING FIELD-SYMBOL(<fs_bseg>).
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    <fs_saida>-bukrs   = <fs_bseg>-bukrs.
    <fs_saida>-werks   = <fs_bseg>-werks.
    <fs_saida>-ebeln   = <fs_bseg>-ebeln.
    <fs_saida>-ebelp   = <fs_bseg>-ebelp.
    <fs_saida>-qtdmiro = <fs_bseg>-menge2.
    <fs_saida>-vlrmiro = <fs_bseg>-dmbtr2.
    <fs_saida>-qtdmigo = <fs_bseg>-menge.
    <fs_saida>-vlrmigo = <fs_bseg>-dmbtr.

    <fs_saida>-diferencaq = <fs_saida>-qtdmigo - <fs_saida>-qtdmiro.
    <fs_saida>-diferencav = <fs_saida>-vlrmigo - <fs_saida>-vlrmiro.

  ENDLOOP.

  IF p_dif IS NOT INITIAL.
    DELETE t_saida WHERE diferencaq = 0 AND diferencav = 0.
    IF t_saida IS INITIAL.
      MESSAGE 'Não há registros com diferença à serem exibidos!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    SORT t_ekbe BY ebeln ebelp vgabe belnr.
    t_ekbe_aux[] = t_ekbe[].
    SORT t_ekbe_aux BY lfbnr vgabe shkzg matnr.
    SORT t_bkpf BY awkey.
    SORT t_bseg BY bukrs belnr gjahr matnr.

    LOOP AT t_saida INTO DATA(w_saida).
      LOOP AT t_ekbe INTO DATA(w_ekbe) WHERE ebeln = w_saida-ebeln
                                       AND   ebelp = w_saida-ebelp.
        CLEAR: w_saida-migo,
               w_saida-anomigo,
               w_saida-dtamigo,
               w_saida-qtdmigo,
               w_saida-vlrmigo.
        CLEAR: w_saida-miro,
               w_saida-anomiro,
               w_saida-dtamiro,
               w_saida-qtdmiro,
               w_saida-vlrmiro.
        IF w_ekbe-vgabe = '1'.
          IF w_ekbe-shkzg = 'H'.
            CONTINUE.
          ENDIF.
          READ TABLE t_ekbe_folha INTO DATA(w_ekbe_folha) WITH KEY belnr = w_ekbe-lfbnr BINARY SEARCH.
          IF sy-subrc = 0.
            IF w_ekbe_folha-dmbtr = 0. "estornado folha
              CONTINUE.
            ELSE.
              w_saida-folha = w_ekbe_folha-belnr.
            ENDIF.
          ENDIF.
          READ TABLE t_ekbe_aux INTO DATA(w_ekbe_aux) WITH KEY lfbnr = w_ekbe-belnr
                                                               vgabe = '1'
                                                               shkzg = 'H'
                                                               matnr = w_ekbe-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY awkey = w_ekbe-awkey BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR: w_saida-vlrmigo,w_saida-qtdmigo.
            LOOP AT t_bseg INTO DATA(w_bseg) WHERE bukrs = w_bkpf-bukrs
                                             AND   belnr = w_bkpf-belnr
                                             AND   gjahr = w_bkpf-gjahr
                                             AND   matnr = w_ekbe-matnr.
              w_saida-vlrmigo = w_saida-vlrmigo + w_bseg-dmbtr.
              w_saida-qtdmigo = w_saida-qtdmigo + w_bseg-menge.
            ENDLOOP.
          ENDIF.
          w_saida-matnr   = w_ekbe-matnr.
          w_saida-migo    = w_ekbe-belnr.
          w_saida-anomigo = w_ekbe-gjahr.
          w_saida-dtamigo = w_ekbe-budat.
          "Achar a MIRO
          READ TABLE t_ekbe_aux INTO w_ekbe_aux WITH KEY lfbnr = w_ekbe-lfbnr
                                                         vgabe = '2'
                                                         shkzg = 'S'
                                                         matnr = w_ekbe-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE t_saida_aux INTO DATA(w_saida_aux2) WITH KEY miro = w_ekbe_aux-belnr.
            IF sy-subrc = 0."Achou a miro correspondente a MIGO
              w_saida-miro     = w_saida_aux2-miro.
              w_saida-anomiro  = w_saida_aux2-anomiro.
              w_saida-dtamiro  = w_saida_aux2-dtamiro.
              w_saida-qtdmiro  = w_saida_aux2-qtdmiro.
              w_saida-vlrmiro  = w_saida_aux2-vlrmiro.
              MODIFY t_saida_aux FROM w_saida INDEX sy-tabix.
            ELSE.
              APPEND w_saida TO t_saida_aux.
            ENDIF.
          ELSE.
            APPEND w_saida TO t_saida_aux.
          ENDIF.
          CLEAR: w_saida-migo,
                 w_saida-folha,
                 w_saida-anomigo,
                 w_saida-dtamigo,
                 w_saida-qtdmigo,
                 w_saida-vlrmigo.
        ELSE.
          IF w_ekbe-shkzg = 'H'.
            CONTINUE.
          ENDIF.
          SELECT SINGLE *
            FROM rbkp
            INTO @DATA(w_rbkp)
            WHERE stblg = @w_ekbe-belnr
            AND   stjah = @w_ekbe-gjahr.

          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

          CLEAR w_ekbe_folha.
          READ TABLE t_ekbe_folha INTO w_ekbe_folha WITH KEY belnr = w_ekbe-lfbnr BINARY SEARCH.
          IF sy-subrc = 0.
            IF w_ekbe_folha-dmbtr = 0. "estornado folha
*              CONTINUE.
            ELSE.
              w_saida-folha = w_ekbe_folha-belnr.
            ENDIF.
          ENDIF.
          READ TABLE t_ekbe_aux INTO w_ekbe_aux WITH KEY lfbnr = w_ekbe-belnr
                                                          vgabe = '2'
                                                          shkzg = 'H'
                                                          matnr = w_ekbe-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          READ TABLE t_bkpf INTO w_bkpf WITH KEY awkey = w_ekbe-awkey BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR: w_saida-vlrmiro,w_saida-qtdmiro.
            LOOP AT t_bseg INTO w_bseg WHERE bukrs = w_bkpf-bukrs
                                       AND   belnr = w_bkpf-belnr
                                       AND   gjahr = w_bkpf-gjahr
                                       AND   matnr = w_ekbe-matnr.
              w_saida-vlrmiro = w_saida-vlrmiro + w_bseg-dmbtr.
              w_saida-qtdmiro = w_saida-qtdmiro + w_bseg-menge.
            ENDLOOP.
          ENDIF.
          w_saida-matnr   = w_ekbe-matnr.
          w_saida-miro    = w_ekbe-belnr.
          w_saida-anomiro = w_ekbe-gjahr.
          w_saida-dtamiro = w_ekbe-budat.
          READ TABLE t_saida_aux INTO DATA(w_saida_aux) WITH KEY  ebeln = w_ekbe-ebeln
                                                                  ebelp = w_ekbe-ebelp
                                                                  matnr = w_ekbe-matnr
                                                                  migo  = w_ekbe-lfbnr.
          IF sy-subrc NE 0.
            READ TABLE t_saida_aux INTO w_saida_aux WITH KEY  ebeln = w_ekbe-ebeln
                                                              ebelp = w_ekbe-ebelp
                                                              matnr = w_ekbe-matnr
                                                              folha = w_saida-folha.
          ENDIF.
          IF sy-subrc = 0."Achou a migo correspondente a MIRO
            w_saida-folha    = w_saida_aux-folha.
            w_saida-migo     = w_saida_aux-migo.
            w_saida-anomigo  = w_saida_aux-anomigo.
            w_saida-dtamigo  = w_saida_aux-dtamigo.
            w_saida-qtdmigo  = w_saida_aux-qtdmigo.
            w_saida-vlrmigo  = w_saida_aux-vlrmigo.
            MODIFY t_saida_aux FROM w_saida INDEX sy-tabix.
          ELSE.
            APPEND w_saida TO t_saida_aux.
          ENDIF.
          CLEAR: w_saida-miro,
                 w_saida-folha,
                 w_saida-anomiro,
                 w_saida-dtamiro,
                 w_saida-qtdmiro,
                 w_saida-vlrmiro.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

    t_saida[] = t_saida_aux[].
    LOOP AT t_saida INTO w_saida.
      w_saida-diferencaq = w_saida-qtdmigo - w_saida-qtdmiro.
      w_saida-diferencav = w_saida-vlrmigo - w_saida-vlrmiro.
      MODIFY  t_saida FROM w_saida INDEX sy-tabix TRANSPORTING diferencaq diferencav.
    ENDLOOP.

    DELETE t_saida WHERE diferencaq = 0 AND diferencav = 0.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_select_data .

  DATA: lv_index         TYPE sy-tabix,
        lv_soma_qtd      TYPE ekbe-menge,
        lv_soma_vlr      TYPE ekbe-dmbtr,
        lr_tipos_pedidos TYPE RANGE OF bsart.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(lt_tvarv)
    WHERE name = 'ZMM_TIPOS_PEDIDO_NAO_TRANSITOR'.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_tvarv ASSIGNING FIELD-SYMBOL(<fs_tvarv>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarv>-low ) TO lr_tipos_pedidos.
    ENDLOOP.
  ENDIF.

*** Seleções MIGO
  SELECT be~ebeln
         be~ebelp
         be~zekkn
         be~belnr
         be~lfbnr
         be~budat
         be~menge
         be~werks
         be~gjahr
         be~dmbtr
         ko~bukrs
         ko~bsart
         be~bwart
    FROM ekbe AS be
    INNER JOIN ekko AS ko
    ON ko~ebeln = be~ebeln
    INNER JOIN ekpo AS po
    ON po~ebeln = be~ebeln
   AND po~ebelp = be~ebelp
    INTO TABLE t_ekbe
    WHERE be~ebeln IN s_ebeln
      AND ko~bukrs IN s_bukrs
      AND po~webre = abap_true
      AND po~werks IN s_werks
      AND be~budat IN s_budat
      AND be~vgabe = '1'
      AND be~shkzg = 'S'
      AND be~bwart NE '123'.

*** Seleções MIGO
  SELECT be~ebeln
         be~ebelp
         be~zekkn
         be~belnr
         be~lfbnr
         be~budat
         be~menge
         be~werks
         be~gjahr
         be~dmbtr
         ko~bukrs
         ko~bsart
         be~bwart
    FROM ekbe AS be
    INNER JOIN ekko AS ko
    ON ko~ebeln = be~ebeln
    INNER JOIN ekpo AS po
    ON po~ebeln = be~ebeln
   AND po~ebelp = be~ebelp
    APPENDING TABLE  t_ekbe
    WHERE be~ebeln IN s_ebeln
      AND ko~bukrs IN s_bukrs
      AND po~webre = abap_true
      AND po~werks IN s_werks
      AND be~budat IN s_budat
      AND be~vgabe = '1'
      AND be~shkzg = 'H'
      AND be~bwart EQ '122'.
  DELETE t_ekbe WHERE bsart IN lr_tipos_pedidos.

  IF t_ekbe IS NOT INITIAL.

    DATA(t_ekbe_aux) = t_ekbe.
    DATA(t_ekbe_aux2) = t_ekbe.

    SORT t_ekbe_aux BY ebeln ebelp belnr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_ekbe_aux COMPARING ebeln ebelp belnr.

    SORT t_ekbe BY ebeln ebelp lfbnr belnr.

    FREE t_ekbe_aux2.

    LOOP AT t_ekbe_aux ASSIGNING FIELD-SYMBOL(<fs_ekbe_aux>).

      READ TABLE t_ekbe TRANSPORTING NO FIELDS
      WITH KEY ebeln = <fs_ekbe_aux>-ebeln
               ebelp = <fs_ekbe_aux>-ebelp
               lfbnr = <fs_ekbe_aux>-lfbnr
               belnr = <fs_ekbe_aux>-belnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        CLEAR lv_soma_vlr.

        LOOP AT t_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>) FROM sy-tabix.
          IF <fs_ekbe_aux>-ebeln <> <fs_ekbe>-ebeln OR
             <fs_ekbe_aux>-ebelp <> <fs_ekbe>-ebelp OR
             <fs_ekbe_aux>-lfbnr <> <fs_ekbe>-lfbnr OR
             <fs_ekbe_aux>-belnr <> <fs_ekbe>-belnr.

            CLEAR lv_soma_vlr.

            EXIT.
          ENDIF.

          APPEND INITIAL LINE TO t_ekbe_aux2 ASSIGNING FIELD-SYMBOL(<fs_ekbe_aux2>).

          lv_soma_vlr = lv_soma_vlr + <fs_ekbe>-dmbtr.

          <fs_ekbe_aux2> = <fs_ekbe>.
          <fs_ekbe_aux2>-dmbtr = lv_soma_vlr.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

    CLEAR lv_soma_vlr.

    SORT t_ekbe_aux2 BY ebeln ebelp zekkn DESCENDING belnr.
    DELETE ADJACENT DUPLICATES FROM t_ekbe_aux2 COMPARING ebeln ebelp belnr.

*    t_ekbe = t_ekbe_aux2.

    IF t_ekbe IS NOT INITIAL.

      SELECT *
        FROM ekbe
        INTO TABLE @DATA(lt_ekbe)
        FOR ALL ENTRIES IN @t_ekbe
        WHERE ebeln = @t_ekbe-ebeln
          AND ebelp = @t_ekbe-ebelp
          AND lfbnr = @t_ekbe-lfbnr
          AND budat IN @s_budat
          AND vgabe = '1'
          AND shkzg = 'H'
          AND bwart NE '122'
          AND belnr > @t_ekbe-belnr.

      SELECT *
      FROM ekbe
      APPENDING TABLE lt_ekbe
      FOR ALL ENTRIES IN t_ekbe
      WHERE ebeln = t_ekbe-ebeln
        AND ebelp = t_ekbe-ebelp
        AND lfbnr = t_ekbe-lfbnr
        AND budat IN s_budat
        AND vgabe = '1'
        AND shkzg = 'S'
        AND bwart EQ '123'
        AND belnr > t_ekbe-belnr.
      SORT lt_ekbe BY ebeln ebelp lfbnr belnr.

      LOOP AT t_ekbe ASSIGNING <fs_ekbe>.
        lv_index = sy-tabix.

        LOOP AT lt_ekbe INTO DATA(wt_ekbe)
                  WHERE ebeln = <fs_ekbe>-ebeln
                  AND   ebelp = <fs_ekbe>-ebelp
                  AND   lfbnr = <fs_ekbe>-lfbnr.
          IF wt_ekbe-belnr > <fs_ekbe>-belnr.
            IF <fs_ekbe>-menge = wt_ekbe-menge.
              DELETE t_ekbe INDEX lv_index.
            ENDIF.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDLOOP.


    ENDIF.

  ENDIF.

  DELETE t_ekbe WHERE bwart = '123'.

  IF t_ekbe IS NOT INITIAL.

    DATA(lt_ekbe_aux) = t_ekbe.
    SORT lt_ekbe_aux BY ebeln ebelp lfbnr.
    DELETE ADJACENT DUPLICATES FROM lt_ekbe_aux COMPARING ebeln ebelp lfbnr.

    SELECT ebeln ebelp lfbnr
      FROM ekbe
      INTO TABLE t_ekbe_folha
      FOR ALL ENTRIES IN lt_ekbe_aux
      WHERE ebeln = lt_ekbe_aux-ebeln
        AND ebelp = lt_ekbe_aux-ebelp
        AND lfbnr = lt_ekbe_aux-lfbnr
        AND vgabe = '9'.
    IF sy-subrc IS INITIAL.
      SORT t_ekbe_folha BY ebeln ebelp lfbnr.
    ENDIF.

  ENDIF.

*** Seleções MIRO

  SELECT be~ebeln
         be~ebelp
         be~zekkn
         be~belnr
         be~lfbnr
         be~budat
         be~menge
         be~werks
         be~gjahr
         be~dmbtr
         ko~bukrs
         ko~bsart
     FROM ekbe AS be
     INNER JOIN rbkp AS rb
     ON  rb~belnr = be~belnr
     AND rb~gjahr = be~gjahr
     AND rb~stblg = ' '
     INNER JOIN ekko AS ko
     ON ko~ebeln = be~ebeln
     INNER JOIN ekpo AS po
     ON  po~ebeln = be~ebeln
     AND po~ebelp = be~ebelp
     INTO TABLE t_ekbe_miro
     WHERE be~ebeln IN s_ebeln
       AND ko~bukrs IN s_bukrs
       AND po~webre = abap_true
       AND po~werks IN s_werks
       AND be~budat IN s_budat
       AND be~vgabe = '2'.
*       AND be~shkzg = 'S'.
  IF sy-subrc IS INITIAL.
    DELETE t_ekbe_miro WHERE bsart IN lr_tipos_pedidos.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_process_data .

  DATA: lv_index TYPE sy-tabix.
  SORT t_ekbe      BY ebeln ebelp lfbnr.
  SORT t_ekbe_miro BY ebeln ebelp lfbnr.
  SORT t_ekbe_folha BY ebeln ebelp lfbnr.
  LOOP AT t_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>).
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    <fs_saida>-bukrs   = <fs_ekbe>-bukrs.
    <fs_saida>-werks   = <fs_ekbe>-werks.
    <fs_saida>-ebeln   = <fs_ekbe>-ebeln.
    <fs_saida>-ebelp   = <fs_ekbe>-ebelp.
    <fs_saida>-migo    = <fs_ekbe>-belnr.
    <fs_saida>-anomigo = <fs_ekbe>-gjahr.
    <fs_saida>-dtamigo = <fs_ekbe>-budat.
    <fs_saida>-qtdmigo = <fs_ekbe>-menge.
    <fs_saida>-vlrmigo = <fs_ekbe>-dmbtr.

    READ TABLE t_ekbe_folha ASSIGNING FIELD-SYMBOL(<fs_folha>)
    WITH KEY ebeln = <fs_ekbe>-ebeln
             ebelp = <fs_ekbe>-ebelp
             lfbnr = <fs_ekbe>-lfbnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida>-folha = <fs_folha>-lfbnr.
    ENDIF.
    CLEAR: <fs_saida>-qtdmiro,<fs_saida>-vlrmiro.
    LOOP AT t_ekbe_miro ASSIGNING FIELD-SYMBOL(<fs_migo>)
                          WHERE    ebeln = <fs_ekbe>-ebeln
                          AND         ebelp = <fs_ekbe>-ebelp
                          AND         lfbnr = <fs_ekbe>-lfbnr.
      <fs_saida>-miro     = <fs_migo>-belnr.
      <fs_saida>-anomiro  = <fs_migo>-gjahr.
      <fs_saida>-dtamiro  = <fs_migo>-budat.
      ADD <fs_migo>-menge TO <fs_saida>-qtdmiro.
      ADD <fs_migo>-dmbtr TO <fs_saida>-vlrmiro.
    ENDLOOP.

    <fs_saida>-diferencaq = <fs_saida>-qtdmigo - <fs_saida>-qtdmiro.
    <fs_saida>-diferencav = <fs_saida>-vlrmigo - <fs_saida>-vlrmiro.

  ENDLOOP.


  LOOP AT t_ekbe_miro ASSIGNING <fs_ekbe>.
    READ TABLE t_saida INTO DATA(w_saida) WITH KEY miro = <fs_ekbe>-belnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    APPEND INITIAL LINE TO t_saida ASSIGNING <fs_saida>.

    <fs_saida>-bukrs    = <fs_ekbe>-bukrs.
    <fs_saida>-werks    = <fs_ekbe>-werks.
    <fs_saida>-ebeln    = <fs_ekbe>-ebeln.
    <fs_saida>-ebelp    = <fs_ekbe>-ebelp.
    <fs_saida>-miro     = <fs_ekbe>-belnr.
    <fs_saida>-anomiro  = <fs_ekbe>-gjahr.
    <fs_saida>-dtamiro  = <fs_ekbe>-budat.
    <fs_saida>-qtdmiro  = <fs_ekbe>-menge.
    <fs_saida>-vlrmiro  = <fs_ekbe>-dmbtr.
    <fs_saida>-diferencaq = <fs_saida>-qtdmigo - <fs_saida>-qtdmiro.
    <fs_saida>-diferencav = <fs_saida>-vlrmigo - <fs_saida>-vlrmiro.

  ENDLOOP.

  IF p_dif IS NOT INITIAL.
    DELETE t_saida WHERE diferencaq = 0 AND diferencav = 0.
    IF t_saida IS INITIAL.
      MESSAGE 'Não há registros com diferença à serem exibidos!' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_process_alv .

  DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
        lv_column     TYPE REF TO cl_salv_column,
        lo_col_tab    TYPE REF TO cl_salv_column_table,
        lo_events     TYPE REF TO cl_salv_events_table,
        lr_selections TYPE REF TO cl_salv_selections,
        lo_alv        TYPE REF TO lc_alv,
        l_text        TYPE string,
        l_icon        TYPE string,
        lr_functions  TYPE REF TO cl_salv_functions_list,
        lr_container  TYPE REF TO cl_gui_custom_container.

  CREATE OBJECT lr_container
    EXPORTING
      container_name = 'CC_ALV'.

  CALL METHOD cl_salv_table=>factory
    EXPORTING
      r_container    = lr_container
      container_name = 'CC_ALV'
      list_display   = if_salv_c_bool_sap=>false
    IMPORTING
      r_salv_table   = gr_alv
    CHANGING
      t_table        = t_saida.

  lr_selections = gr_alv->get_selections( ).
  lr_selections->set_selection_mode( 3 ).

  lr_columns = gr_alv->get_columns( ).

  lo_col_tab ?= lr_columns->get_column( 'EBELN' ).
  CALL METHOD lo_col_tab->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.

  lv_column = lr_columns->get_column( 'EBELN' ).
  lv_column->set_long_text( 'Pedido' ).
  lv_column->set_medium_text( 'Pedido' ).
  lv_column->set_short_text( 'Pedido' ).

  lo_col_tab ?= lr_columns->get_column( 'MIGO' ).
  CALL METHOD lo_col_tab->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.

  lv_column = lr_columns->get_column( 'MIGO' ).
  lv_column->set_long_text( 'Migo' ).
  lv_column->set_medium_text( 'Migo' ).
  lv_column->set_short_text( 'Migo' ).

  lv_column = lr_columns->get_column( 'ANOMIGO' ).
  lv_column->set_long_text( 'Ano Migo' ).
  lv_column->set_medium_text( 'Ano Migo' ).
  lv_column->set_short_text( 'Ano Migo' ).

  lv_column = lr_columns->get_column( 'DTAMIGO' ).
  lv_column->set_long_text( 'Dta. Migo' ).
  lv_column->set_medium_text( 'Dta. Migo' ).
  lv_column->set_short_text( 'Dta. Migo' ).

  lv_column = lr_columns->get_column( 'QTDMIGO' ).
  lv_column->set_long_text( 'Qtd. Migo' ).
  lv_column->set_medium_text( 'Qtd. Migo' ).
  lv_column->set_short_text( 'Qtd. Migo' ).

  lv_column = lr_columns->get_column( 'VLRMIGO' ).
  lv_column->set_long_text( 'Vlr. Migo' ).
  lv_column->set_medium_text( 'Vlr. Migo' ).
  lv_column->set_short_text( 'Vlr. Migo' ).

  lo_col_tab ?= lr_columns->get_column( 'FOLHA' ).
  CALL METHOD lo_col_tab->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.

  lv_column = lr_columns->get_column( 'FOLHA' ).
  lv_column->set_long_text( 'Folha' ).
  lv_column->set_medium_text( 'Folha' ).
  lv_column->set_short_text( 'Folha' ).

  lv_column = lr_columns->get_column( 'MIRO' ).

  lo_col_tab ?= lr_columns->get_column( 'MIRO' ).
  CALL METHOD lo_col_tab->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.

  lv_column->set_long_text( 'Miro' ).
  lv_column->set_medium_text( 'Miro' ).
  lv_column->set_short_text( 'Miro' ).

  lv_column = lr_columns->get_column( 'ANOMIRO' ).
  lv_column->set_long_text( 'Ano Miro' ).
  lv_column->set_medium_text( 'Ano Miro' ).
  lv_column->set_short_text( 'Ano Miro' ).

  lv_column = lr_columns->get_column( 'DTAMIRO' ).
  lv_column->set_long_text( 'Dta. Miro' ).
  lv_column->set_medium_text( 'Dta. Miro' ).
  lv_column->set_short_text( 'Dta. Miro' ).

  lv_column = lr_columns->get_column( 'QTDMIRO' ).
  lv_column->set_long_text( 'Qtd. Miro' ).
  lv_column->set_medium_text( 'Qtd. Miro' ).
  lv_column->set_short_text( 'Qtd. Miro' ).

  lv_column = lr_columns->get_column( 'VLRMIRO' ).
  lv_column->set_long_text( 'Vlr. Miro' ).
  lv_column->set_medium_text( 'Vlr. Miro' ).
  lv_column->set_short_text( 'Vlr. Miro' ).

  lv_column = lr_columns->get_column( 'DIFERENCAQ' ).
  lv_column->set_long_text( 'Qtd. diferença MIGO' ).
  lv_column->set_medium_text( 'Qtd. diferença MIGO' ).
  lv_column->set_short_text( 'QtdDifMIGO' ).

  lv_column = lr_columns->get_column( 'DIFERENCAV' ).
  lv_column->set_long_text( 'Valor. diferença MIGO' ).
  lv_column->set_medium_text( 'Valor. difer. MIGO' ).
  lv_column->set_short_text( 'ValDifMIGO' ).
*   all events
  lo_events = gr_alv->get_event( ).


  lr_functions = gr_alv->get_functions( ).
  lr_functions->set_default( abap_true ).

*... §3.2 include own functions
  TRY.
      l_text = 'Estorno Migo/Folha'.
*      l_icon = icon_complete.
      lr_functions->add_function(
        name     = 'ESTORMIGO'
        icon     = l_icon
        text     = l_text
        tooltip  = l_text
        position = if_salv_c_function_position=>right_of_salv_functions ).
    CATCH cx_salv_wrong_call cx_salv_existing.
  ENDTRY.

  TRY.
      l_text = 'Estorno Miro'.
*      l_icon = icon_complete.
      lr_functions->add_function(
        name     = 'ESTORMIRO'
        icon     = l_icon
        text     = l_text
        tooltip  = l_text
        position = if_salv_c_function_position=>right_of_salv_functions ).
    CATCH cx_salv_wrong_call cx_salv_existing.
  ENDTRY.

  CREATE OBJECT lo_alv.

*   event handler
  SET HANDLER lo_alv->on_link_click FOR lo_events.
*  SET HANDLER lo_alv->handle_toolbar FOR gr_alv.
  SET HANDLER lo_alv->on_user_command FOR lo_events.

  gr_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_FOLHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_SAIDA>_FOLHA  text
*----------------------------------------------------------------------*
FORM f_estorno_folha  USING p_saida_folha.

  DATA:
       lt_bdc TYPE TABLE OF bdcdata.
  DATA:
    lw_bdc           TYPE bdcdata,
    lw_ctu_params    TYPE ctu_params,
    lt_shdb_messages TYPE tab_bdcmsgcoll,
    lt_return        TYPE TABLE OF bapiret2.
*------------------------------------------------

  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0400'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=SELP'.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0340'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=ENTE'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'RM11R-LBLNI'.
  lw_bdc-fval     = p_saida_folha.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0400'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=AKCH'.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0400'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=ACCR'.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0400'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=SAVE'.
  APPEND lw_bdc TO lt_bdc.
**-------------------------------
  "Popup confirm
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLSPO1'.
  lw_bdc-dynpro   = '0300'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=YES'.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------
  CLEAR: lw_bdc.
  lw_bdc-dynbegin = 'X'.
  lw_bdc-program  = 'SAPLMLSR'.
  lw_bdc-dynpro   = '0110'.
  APPEND lw_bdc TO lt_bdc.

  CLEAR: lw_bdc.
  lw_bdc-fnam     = 'BDC_OKCODE'.
  lw_bdc-fval     = '=ENTE'.
  APPEND lw_bdc TO lt_bdc.
*-------------------------------

  lw_ctu_params-dismode = 'N'.
  lw_ctu_params-updmode = 'S'.

  CALL TRANSACTION 'ML81N'
             USING lt_bdc
           OPTIONS FROM lw_ctu_params
          MESSAGES INTO lt_shdb_messages.

  COMMIT WORK AND WAIT.

  DO 10 TIMES.

    SELECT lblni,kzabn
         INTO TABLE @DATA(lt_essr)
         FROM essr
         WHERE lblni = @p_saida_folha.
    IF sy-subrc IS NOT INITIAL.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  LOOP AT lt_essr ASSIGNING FIELD-SYMBOL(<fs_essr>).

    "----------------------------------------
    " DELETE SERV.ENTRYSHEET
    "----------------------------------------
    CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
      EXPORTING
        entrysheet = <fs_essr>-lblni
      TABLES
        return     = lt_return.

    DELETE lt_return WHERE type <> 'E'.
    IF lt_return IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      MESSAGE 'Folha ' && space && p_saida_folha && ' estornada com sucesso' TYPE 'I'.

    ELSE.

      SORT lt_return BY type.
      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
      IF sy-subrc IS INITIAL.
        MESSAGE ID <fs_return>-id TYPE 'I' NUMBER <fs_return>-number WITH <fs_return>-message_v1.
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_MIGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_SAIDA>_MIGO  text
*----------------------------------------------------------------------*
FORM f_estorno_migo  USING p_migo
                           p_ano.

  DATA: lw_header_ret TYPE bapi2017_gm_head_ret,
        lt_return     TYPE TABLE OF bapiret2.

  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument = p_migo
      matdocumentyear  = p_ano
    IMPORTING
      goodsmvt_headret = lw_header_ret
    TABLES
      return           = lt_return.
  IF lt_return IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    MESSAGE 'Migo ' && space && p_migo && ' estornada com sucesso!' TYPE 'I'.

  ELSE.

    SORT lt_return BY type.
    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
    IF sy-subrc IS INITIAL.
      MESSAGE ID <fs_return>-id TYPE 'I' NUMBER <fs_return>-number WITH <fs_return>-message_v1.
    ENDIF.
  ENDIF.

ENDFORM.
