FUNCTION z_mm_get_pedido_compra_psq.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  EWERK OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_EBELN) TYPE  EBELN OPTIONAL
*"     REFERENCE(I_EBELP) TYPE  EBELP OPTIONAL
*"     REFERENCE(I_BSTYP) TYPE  EBSTYP OPTIONAL
*"     REFERENCE(I_BSART) TYPE  ESART OPTIONAL
*"     REFERENCE(I_MENGE_GE) TYPE  BSTMG OPTIONAL
*"     REFERENCE(I_MEINS) TYPE  BSTME OPTIONAL
*"     REFERENCE(I_ZMMT0075) TYPE  ZDE_ZMMT0075_T OPTIONAL
*"     REFERENCE(I_ABRIR_TELA) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(I_MATNR_T) TYPE  EHS_MATNR_T OPTIONAL
*"     REFERENCE(I_FILTRO_1) TYPE  ZDE_EKPO_HELP_SALDO_T OPTIONAL
*"  EXPORTING
*"     REFERENCE(R_EKPO) TYPE  EKPO
*"     REFERENCE(R_SALDO_ITEM) TYPE  ZDE_EKPO_HELP_SALDO
*"     REFERENCE(E_EKPO_T) TYPE  ZDE_EKPO_HELP_SALDO_T
*"  EXCEPTIONS
*"      NAO_ENCONTRADO_PEDIDO
*"----------------------------------------------------------------------

  DATA: qtd_pedidos_itens TYPE i,
        centro_real       TYPE  werks_d.

  gb_menge_saldo = i_menge_ge.

  CLEAR: it_pedidos_itens[], it_pedidos_itens,
         it_pedidos_historico[], it_pedidos_historico.

  DATA: r_lifnr TYPE RANGE OF lifnr,
        r_bukrs TYPE RANGE OF bukrs,
        r_werks TYPE RANGE OF ewerk,
        r_matnr TYPE RANGE OF matnr,
        r_lgort TYPE RANGE OF lgort_d,
        r_ebeln TYPE RANGE OF ebeln,
        r_ebelp TYPE RANGE OF ebelp,
        r_bstyp TYPE RANGE OF ebstyp,
        r_bsart TYPE RANGE OF esart,
        r_charg TYPE RANGE OF charg_d.

  DATA: w_lifnr LIKE LINE OF r_lifnr,
        w_bukrs LIKE LINE OF r_bukrs,
        w_werks LIKE LINE OF r_werks,
        w_matnr LIKE LINE OF r_matnr,
        w_lgort LIKE LINE OF r_lgort,
        w_ebeln LIKE LINE OF r_ebeln,
        w_ebelp LIKE LINE OF r_ebelp,
        w_bstyp LIKE LINE OF r_bstyp,
        w_bsart LIKE LINE OF r_bsart.

  DATA: r_empresa TYPE RANGE OF bukrs.
  DATA: r_bsart_ TYPE RANGE OF bsart.

  IF i_lifnr IS NOT INITIAL.
    w_lifnr-sign   = 'I'.
    w_lifnr-option = 'EQ'.
    w_lifnr-low    = i_lifnr.
    w_lifnr-high   = i_lifnr.
    APPEND w_lifnr TO r_lifnr.

    SELECT SINGLE * INTO @DATA(wa_lfa1)
      FROM lfa1
     WHERE lifnr EQ @i_lifnr.

    IF wa_lfa1-stcd1 IS NOT INITIAL.

      CONCATENATE wa_lfa1-stcd1(8) '%' INTO wa_lfa1-stcd1.

      SELECT * INTO TABLE @DATA(it_lifnr)
        FROM lfa1
       WHERE stcd1 LIKE @wa_lfa1-stcd1
         AND lifnr NE @i_lifnr.

      LOOP AT it_lifnr INTO DATA(wa_fornecedor).
        w_lifnr-low    = wa_fornecedor-lifnr.
        w_lifnr-high   = wa_fornecedor-lifnr.
        APPEND w_lifnr TO r_lifnr.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF i_bukrs IS NOT INITIAL.
    w_bukrs-sign   = 'I'.
    w_bukrs-option = 'EQ'.
    w_bukrs-low    = i_bukrs.
    w_bukrs-high   = i_bukrs.
    APPEND w_bukrs TO r_bukrs.
  ELSE.
    LOOP AT i_filtro_1 INTO DATA(wa_filtro_1).
      IF wa_filtro_1-bukrs IS NOT INITIAL.
        w_bukrs-sign   = 'I'.
        w_bukrs-option = 'EQ'.
        w_bukrs-low    = wa_filtro_1-bukrs.
        w_bukrs-high   = wa_filtro_1-bukrs.
        APPEND w_bukrs TO r_bukrs.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF i_werks IS NOT INITIAL.
    w_werks-sign   = 'I'.
    w_werks-option = 'EQ'.
    w_werks-low    = i_werks.
    w_werks-high   = i_werks.
    APPEND w_werks TO r_werks.
  ELSE.
    LOOP AT i_filtro_1 INTO wa_filtro_1.
      IF wa_filtro_1-werks IS NOT INITIAL.
        w_werks-sign   = 'I'.
        w_werks-option = 'EQ'.
        w_werks-low    = wa_filtro_1-werks.
        w_werks-high   = wa_filtro_1-werks.
        APPEND w_werks TO r_werks.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF i_matnr IS NOT INITIAL.
    w_matnr-sign   = 'I'.
    w_matnr-option = 'EQ'.
    w_matnr-low    = i_matnr.
    w_matnr-high   = i_matnr.
    APPEND w_matnr TO r_matnr.
  ELSE.
    LOOP AT i_filtro_1 INTO wa_filtro_1.
      IF wa_filtro_1-matnr IS NOT INITIAL.
        w_matnr-sign   = 'I'.
        w_matnr-option = 'EQ'.
        w_matnr-low    = wa_filtro_1-matnr.
        w_matnr-high   = wa_filtro_1-matnr.
        APPEND w_matnr TO r_matnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT i_matnr_t INTO DATA(wa_matnr).
    w_matnr-sign   = 'I'.
    w_matnr-option = 'EQ'.
    w_matnr-low    = wa_matnr-matnr.
    w_matnr-high   = wa_matnr-matnr.
    APPEND w_matnr TO r_matnr.
  ENDLOOP.

  IF i_lgort IS NOT INITIAL.
    w_lgort-sign   = 'I'.
    w_lgort-option = 'EQ'.
    w_lgort-low    = i_lgort.
    w_lgort-high   = i_lgort.
    APPEND w_lgort TO r_lgort.
  ENDIF.

  IF i_ebeln IS NOT INITIAL.
    w_ebeln-sign   = 'I'.
    w_ebeln-option = 'EQ'.
    w_ebeln-low    = |{ i_ebeln ALPHA = IN }|.
    w_ebeln-high   = |{ i_ebeln ALPHA = IN }|.
    APPEND w_ebeln TO r_ebeln.
  ELSE.
    LOOP AT i_filtro_1 INTO wa_filtro_1.
      IF wa_filtro_1-ebeln IS NOT INITIAL.
        w_ebeln-sign   = 'I'.
        w_ebeln-option = 'EQ'.
        w_ebeln-low    = |{ wa_filtro_1-ebeln ALPHA = IN }|.
        w_ebeln-high   = |{ wa_filtro_1-ebeln ALPHA = IN }|.
        APPEND w_ebeln TO r_ebeln.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF i_ebelp IS NOT INITIAL.
    w_ebelp-sign   = 'I'.
    w_ebelp-option = 'EQ'.
    w_ebelp-low    = i_ebelp.
    w_ebelp-high   = i_ebelp.
    APPEND w_ebelp TO r_ebelp.
  ELSE.
    LOOP AT i_filtro_1 INTO wa_filtro_1.
      IF wa_filtro_1-ebelp IS NOT INITIAL.
        w_ebelp-sign   = 'I'.
        w_ebelp-option = 'EQ'.
        w_ebelp-low    = wa_filtro_1-ebelp.
        w_ebelp-high   = wa_filtro_1-ebelp.
        APPEND w_ebelp TO r_ebelp.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF i_bstyp IS NOT INITIAL.
    w_bstyp-sign   = 'I'.
    w_bstyp-option = 'EQ'.
    w_bstyp-low    = i_bstyp.
    w_bstyp-high   = i_bstyp.
    APPEND w_bsart TO r_bstyp.
  ENDIF.

  IF i_bsart IS NOT INITIAL.
    w_bsart-sign   = 'I'.
    w_bsart-option = 'EQ'.
    w_bsart-low    = i_bsart.
    w_bsart-high   = i_bsart.
    APPEND w_bsart TO r_bsart.
  ENDIF.

  LOOP AT i_zmmt0075 INTO DATA(wa_zmmt0075).
    w_bstyp-sign   = 'I'.
    w_bstyp-option = 'EQ'.
    w_bstyp-low    = wa_zmmt0075-bstyp.
    w_bstyp-high   = wa_zmmt0075-bstyp.
    APPEND w_bsart TO r_bstyp.

    w_bsart-sign   = 'I'.
    w_bsart-option = 'EQ'.
    w_bsart-low    = wa_zmmt0075-bsart.
    w_bsart-high   = wa_zmmt0075-bsart.
    APPEND w_bsart TO r_bsart.
  ENDLOOP.

  IF i_charg IS NOT INITIAL.
    r_charg = VALUE #( sign = 'I' option = 'EQ' ( low = i_charg high = i_charg ) ).
  ENDIF.


  IF sy-batch EQ abap_false.
    CALL FUNCTION 'TH_REDISPATCH'.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = 'Pesquisando Cabeçalho(s) de Pedido(s) - Fornecedor para o Pedido!'.
  ENDIF.


  "=====================================Inicio USER STORY 72906"  / Anderson Oenning - 20/04/2022
  "Seleciona empresa do pedido.
  IF r_ebeln[] IS NOT INITIAL.
    SELECT * FROM massekko INTO TABLE @DATA(t_massekko) WHERE ebeln IN @r_ebeln.
    IF t_massekko IS NOT INITIAL.
      r_bsart_ = VALUE #( FOR l IN t_massekko ( option = 'EQ' sign = 'I' low = l-bsart ) ).
    ENDIF.
  ENDIF.

  "Verifica se existe empresa do pedido no SET.
  SELECT  *
   FROM setleaf
   INTO TABLE @DATA(i_data)
     WHERE setname EQ 'MAGGI_PEDIDOS_COUPA'
       AND valfrom IN @r_bsart_.

  IF i_data IS INITIAL.
    SELECT * INTO TABLE @DATA(it_massekko)
       FROM massekko AS k
      WHERE ebeln      IN @r_ebeln
        AND lifnr      IN @r_lifnr "Fornecedor do Pedido
        AND bukrs      IN @r_bukrs
        AND bsart      IN @r_bsart
        AND frgrl      NE @abap_true
      ORDER BY ebeln.
  ELSE.
    FREE: it_massekko.
    SELECT * INTO TABLE it_massekko
   FROM massekko AS k
  WHERE ebeln      IN r_ebeln
    AND lifnr      IN r_lifnr "Fornecedor do Pedido
    AND bukrs      IN r_bukrs
    AND bsart      IN r_bsart
*     AND frgrl      NE @abap_true
    ORDER BY ebeln.
  ENDIF.


  IF sy-batch EQ abap_false.
    CALL FUNCTION 'TH_REDISPATCH'.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = 'Pesquisando Cabeçalho(s) de Pedido(s) - Fornecedor para a remessa!'.
  ENDIF.

  IF i_data IS INITIAL.
    SELECT * APPENDING TABLE it_massekko
       FROM massekko AS k
      WHERE ebeln      IN r_ebeln
        AND llief      IN r_lifnr "Fornecedor para Remessa
        AND bukrs      IN r_bukrs
        AND bsart      IN r_bsart
        AND frgrl      NE abap_true
      ORDER BY ebeln.
  ELSE.
    SELECT * APPENDING TABLE it_massekko
       FROM massekko AS k
      WHERE ebeln      IN r_ebeln
        AND llief      IN r_lifnr "Fornecedor para Remessa
        AND bukrs      IN r_bukrs
        AND bsart      IN r_bsart
*        AND frgrl      NE abap_true
      ORDER BY ebeln.
  ENDIF.
  "=====================================Fim USER STORY 72906"  / Anderson Oenning - 20/04/2022

  IF it_massekko IS NOT INITIAL.

    IF sy-batch EQ abap_false.
      CALL FUNCTION 'TH_REDISPATCH'.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = 'Pesquisando Item(s) de Pedido(s)!'.
    ENDIF.

    IF i_charg IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_massekpo)
        FROM massekpo AS p
         FOR ALL ENTRIES IN @it_massekko
       WHERE ebeln      EQ @it_massekko-ebeln
         AND ebelp      IN @r_ebelp
         AND werks      IN @r_werks
         AND matnr      IN @r_matnr
         AND loekz      EQ @space
         AND matnr      NE @space
         AND EXISTS ( SELECT * FROM eket AS k WHERE k~ebeln EQ p~ebeln AND k~ebelp EQ p~ebelp AND k~charg IN @r_charg ).
    ELSE.

      SELECT * INTO TABLE @it_massekpo
        FROM massekpo AS p
         FOR ALL ENTRIES IN @it_massekko
       WHERE ebeln      EQ @it_massekko-ebeln
         AND ebelp      IN @r_ebelp
         AND werks      IN @r_werks
         AND matnr      IN @r_matnr
         AND loekz      EQ @space
         AND matnr      NE @space.
    ENDIF.
  ENDIF.
  "MASSEKKO
  "MASSEKPO

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e001 RAISING nao_encontrado_pedido.
  ELSE.
    SORT it_massekko BY ebeln.
    DELETE ADJACENT DUPLICATES FROM it_massekko COMPARING ebeln.

    SORT it_massekpo BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM it_massekpo COMPARING ebeln ebelp.

    SELECT * INTO TABLE @DATA(it_mara)
      FROM mara
      FOR ALL ENTRIES IN @it_massekpo
     WHERE matnr EQ @it_massekpo-matnr.

    SORT it_mara BY matnr.
  ENDIF.

  IF sy-batch EQ abap_false.
    CALL FUNCTION 'TH_REDISPATCH'.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = 'Ajustando Saldo do(s) Pedido(s)!'.
  ENDIF.

  LOOP AT it_massekko INTO DATA(wa_massekko).
    LOOP AT it_massekpo INTO DATA(wa_massekpo) WHERE ebeln EQ wa_massekko-ebeln.

      SELECT * INTO TABLE @DATA(it_mlhelp_po_ekbe)
        FROM mlhelp_po_ekbe AS k
       WHERE ebeln      EQ @wa_massekpo-ebeln
         AND ebelp      EQ @wa_massekpo-ebelp
         AND vgabe      EQ '1'
*         AND SHKZG      EQ 'S'
*         AND NOT EXISTS ( SELECT * FROM MSEG AS M WHERE M~SJAHR = K~GJAHR AND M~SMBLN = K~BELNR AND M~SMBLP = K~BUZEI )
       ORDER BY cpudt.

      "Adiciona o Item
      CLEAR: it_pedidos_itens.
      it_pedidos_itens-ebeln       = wa_massekpo-ebeln.
      it_pedidos_itens-bukrs       = wa_massekko-bukrs.
      it_pedidos_itens-bsart       = wa_massekko-bsart.
      it_pedidos_itens-ekorg       = wa_massekko-ekorg.
      it_pedidos_itens-waers_ekko  = wa_massekko-waers.
      it_pedidos_itens-wkurs       = wa_massekko-wkurs.
      it_pedidos_itens-ebelp       = wa_massekpo-ebelp.
      it_pedidos_itens-werks       = wa_massekpo-werks.
      it_pedidos_itens-ernam       = wa_massekko-ernam.

      READ TABLE it_mara WITH KEY matnr = wa_massekpo-matnr INTO DATA(wa_mara) BINARY SEARCH.

      IF wa_mara-meins NE wa_massekpo-meins.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = wa_massekpo-matnr
            i_in_me              = wa_massekpo-meins
            i_out_me             = wa_mara-meins
            i_menge              = wa_massekpo-menge
          IMPORTING
            e_menge              = wa_massekpo-menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          "005  Material &1 não pode ser convertido de &2 para &3!
          MESSAGE e005 WITH wa_massekpo-matnr wa_massekpo-meins wa_mara-meins RAISING nao_encontrado_pedido.
        ENDIF.
        wa_massekpo-meins = wa_mara-meins.

      ENDIF.

      it_pedidos_itens-matnr       = wa_massekpo-matnr.
      it_pedidos_itens-meins       = wa_massekpo-meins.
      it_pedidos_itens-menge_ekpo  = wa_massekpo-menge.
      it_pedidos_itens-menge_ekbe  = 0.
      it_pedidos_itens-menge_saldo = wa_massekpo-menge.
      it_pedidos_itens-netwr       = wa_massekpo-netwr.
      it_pedidos_itens-netpr       = wa_massekpo-netpr.
      it_pedidos_itens-brtwr       = wa_massekpo-brtwr.
      it_pedidos_itens-brtwr       = wa_massekpo-brtwr.
      it_pedidos_itens-navnw       = wa_massekpo-navnw.
      APPEND it_pedidos_itens.

      "Percorre o Histórico
      LOOP AT it_mlhelp_po_ekbe INTO DATA(wa_mlhelp_po_ekbe) WHERE ebeln EQ wa_massekpo-ebeln AND ebelp = wa_massekpo-ebelp.
        CLEAR: it_pedidos_historico.
        READ TABLE it_pedidos_itens WITH KEY ebeln = wa_mlhelp_po_ekbe-ebeln ebelp = wa_mlhelp_po_ekbe-ebelp ASSIGNING FIELD-SYMBOL(<fs_item>).
        IF wa_mlhelp_po_ekbe-shkzg = 'S'.
          ADD wa_mlhelp_po_ekbe-menge_ekbe TO <fs_item>-menge_ekbe.
          SUBTRACT wa_mlhelp_po_ekbe-menge_ekbe FROM <fs_item>-menge_saldo.
*          <FS_ITEM>-MENGE_SALDO = <FS_ITEM>-MENGE_EKPO - <FS_ITEM>-MENGE_EKBE.
        ELSE.
          SUBTRACT wa_mlhelp_po_ekbe-menge_ekbe FROM <fs_item>-menge_ekbe.
          ADD wa_mlhelp_po_ekbe-menge_ekbe TO <fs_item>-menge_saldo.
        ENDIF.
        MOVE-CORRESPONDING <fs_item> TO it_pedidos_historico.
        APPEND it_pedidos_historico.
      ENDLOOP.

    ENDLOOP.
  ENDLOOP.

  LOOP AT i_filtro_1 INTO wa_filtro_1 WHERE menge_saldo IS NOT INITIAL.
    IF wa_filtro_1-werks IS NOT INITIAL AND wa_filtro_1-matnr IS NOT INITIAL.
      DELETE it_pedidos_historico WHERE bukrs EQ wa_filtro_1-bukrs AND werks EQ wa_filtro_1-werks AND matnr EQ wa_filtro_1-matnr AND menge_saldo LT wa_filtro_1-menge_saldo.
    ELSEIF wa_filtro_1-matnr IS NOT INITIAL.
      DELETE it_pedidos_historico WHERE matnr EQ wa_filtro_1-matnr AND menge_saldo LT wa_filtro_1-menge_saldo.
    ENDIF.
  ENDLOOP.

  DELETE it_pedidos_itens WHERE menge_saldo LE 0.

  DESCRIBE TABLE it_pedidos_itens LINES qtd_pedidos_itens.

  MOVE it_pedidos_itens[] TO e_ekpo_t[].

  IF qtd_pedidos_itens EQ 1.
    READ TABLE it_pedidos_itens INDEX 1 INTO DATA(wa_pedidos_itens).
    PERFORM seleciona_pedido USING wa_pedidos_itens.
    IF sy-subrc IS INITIAL.
      r_ekpo = gb_ekpo.
      EXIT.
    ENDIF.
  ENDIF.

  CHECK i_abrir_tela EQ abap_true.

  ck_selecionou_pedido = abap_false.

  LOOP AT it_pedidos_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).
    IF <fs_itens>-menge_saldo GE gb_menge_saldo.
      <fs_itens>-rowcolor = 'C501'.
    ELSE.
      <fs_itens>-rowcolor = 'C601'.
    ENDIF.
  ENDLOOP.

  CLEAR: zde_ekpo_help_cab.

  IF i_lifnr IS NOT INITIAL.
    zde_ekpo_help_cab-lifnr = wa_lfa1-lifnr.
    zde_ekpo_help_cab-name1 = wa_lfa1-name1.
  ENDIF.

  IF i_bukrs IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_t001)
      FROM t001
     WHERE bukrs EQ @i_bukrs.

    zde_ekpo_help_cab-bukrs = wa_t001-bukrs.
    zde_ekpo_help_cab-butxt = wa_t001-butxt.
  ENDIF.

  IF i_werks IS NOT INITIAL.
    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = i_werks
      IMPORTING
        centro_real          = centro_real
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE branch EQ @centro_real.

    zde_ekpo_help_cab-branch = wa_j_1bbranch-branch.
    zde_ekpo_help_cab-name   = wa_j_1bbranch-name.
  ENDIF.

  IF i_matnr IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_makt)
      FROM makt
     WHERE spras EQ @sy-langu
       AND matnr EQ @i_matnr.

    zde_ekpo_help_cab-matnr = wa_makt-matnr.
    zde_ekpo_help_cab-maktg = wa_makt-maktg.
  ENDIF.

  zde_ekpo_help_cab-menge_ekpo = i_menge_ge.
  zde_ekpo_help_cab-meins      = i_meins.

  CALL SCREEN 9001 STARTING AT 25 5.

  IF ck_selecionou_pedido EQ abap_true.
    r_ekpo = gb_ekpo.
    r_saldo_item = gb_saldo_item.
  ENDIF.

  FREE: i_data, t_massekko.

ENDFUNCTION.
