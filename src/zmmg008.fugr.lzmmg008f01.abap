*----------------------------------------------------------------------*
***INCLUDE LZMMG008F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_seleciona_dados CHANGING e_erro.
  REFRESH: it_ausp, it_ausp_2, it_ausp_3, it_ausp_4.

  IF s_werks IS NOT INITIAL.

    SELECT *
      FROM t001w
      INTO TABLE it_t001w
    WHERE werks IN s_werks.

    "Pedidos
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMMCCUSTOPO'
      IMPORTING
        output = vg_atinn.

    CALL FUNCTION 'CLSE_SELECT_AUSP'
      EXPORTING
        klart                     = '032'
        atinn                     = vg_atinn
      TABLES
        t_ausp                    = it_ausp_2
      EXCEPTIONS
        no_entry_found            = 1
        parameters_not_sufficient = 2
        OTHERS                    = 3.

    IF sy-subrc <> 0.
      e_erro = 'ERRO AO CONVERTEER A CARACTERÍSTICA'.
      EXIT.
    ENDIF.

    "Centro
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMMCENTRO'
      IMPORTING
        output = vg_atinn.

    CALL FUNCTION 'CLSE_SELECT_AUSP'
      EXPORTING
        klart                     = '032'
        atinn                     = vg_atinn
      TABLES
        t_ausp                    = it_ausp_3
      EXCEPTIONS
        no_entry_found            = 1
        parameters_not_sufficient = 2
        OTHERS                    = 3.

    IF sy-subrc <> 0.
      e_erro = 'ERRO AO CONVERTER A CARACTERÍSTICA'.
      EXIT.
    ENDIF.

    "Tipos
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMMTIPODOC'
      IMPORTING
        output = vg_atinn.

    CALL FUNCTION 'CLSE_SELECT_AUSP'
      EXPORTING
        klart                     = '032'
        atinn                     = vg_atinn
      TABLES
        t_ausp                    = it_ausp_4
      EXCEPTIONS
        no_entry_found            = 1
        parameters_not_sufficient = 2
        OTHERS                    = 3.

    IF sy-subrc <> 0.
      e_erro = 'ERRO AO CONVERTER A CARACTERÍSTICA'.
      EXIT.
    ENDIF.

  ELSE.

    SELECT *
           FROM csks
           INTO TABLE it_csks
           WHERE kostl IN s_kostl
             AND datbi GE sy-datum
             AND gsber IN s_werks
             ORDER BY kostl.

    SELECT *
      FROM t001
      INTO TABLE it_t001
      FOR ALL ENTRIES IN it_csks
    WHERE bukrs EQ it_csks-bukrs.


    SELECT *
      FROM t001w
      INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_csks
    WHERE werks EQ it_csks-gsber.


    SELECT *
      INTO TABLE it_cskt
      FROM cskt
       FOR ALL ENTRIES IN it_csks
     WHERE kokrs EQ it_csks-kokrs
       AND kostl EQ it_csks-kostl
    AND datbi EQ it_csks-datbi.

    SELECT *
      FROM zmmt0003
      INTO TABLE it_zmmt0003
      FOR ALL ENTRIES IN it_csks
    WHERE kostl EQ it_csks-kostl
    AND   dt_val_de  LE sy-datum
    AND   dt_val_ate GE sy-datum.

    "Requisiçoes
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMMCCUSTO'
      IMPORTING
        output = vg_atinn.

    CALL FUNCTION 'CLSE_SELECT_AUSP'
      EXPORTING
        klart                     = '032'
        atinn                     = vg_atinn
      TABLES
        t_ausp                    = it_ausp
      EXCEPTIONS
        no_entry_found            = 1
        parameters_not_sufficient = 2
        OTHERS                    = 3.
    IF sy-subrc <> 0.
      e_erro = 'ERRO AO CONVERTER A CARACTERÍSTICA'.
      EXIT.
    ENDIF.

    "Pedidos
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMMCCUSTOPO'
      IMPORTING
        output = vg_atinn.

    CALL FUNCTION 'CLSE_SELECT_AUSP'
      EXPORTING
        klart                     = '032'
        atinn                     = vg_atinn
      TABLES
        t_ausp                    = it_ausp_2
      EXCEPTIONS
        no_entry_found            = 1
        parameters_not_sufficient = 2
        OTHERS                    = 3.

    IF sy-subrc <> 0.
      e_erro = 'ERRO AO CONVERTER A CARACTERÍSTICA'.
      EXIT.
    ENDIF.

  ENDIF.


  SELECT *
     FROM t16fs
  INTO TABLE it_t16fs.

  SORT it_t16fs BY frggr frgsx.

  SELECT *
    FROM t16ft
    INTO TABLE it_t16ft
  WHERE spras EQ 'P'.
  SORT it_t16ft BY frggr frgsx.

  SELECT *
    FROM t16fc
  INTO TABLE it_t16fc.
  SORT it_t16fc BY frggr frgco.

  SELECT *
    FROM t16fd
  INTO TABLE it_t16fd.
  SORT it_t16fd BY frggr frgco.

  IF ( it_zmmt0003[] IS NOT INITIAL ).

    SELECT *
      FROM user_addr
      INTO TABLE it_user_addr
      FOR ALL ENTRIES IN it_zmmt0003[]
      WHERE bname = it_zmmt0003-uname.
    SORT it_user_addr BY name_textc bname.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_ORGANIZA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_organiza_saida CHANGING it_saida TYPE zmme0111_t.

  DATA: aux_atwrt	       TYPE atwrt,
        vg_subrc_ausp_01 TYPE sysubrc,
        vg_subrc_ausp_02 TYPE sysubrc,
        vg_sy_tabix      TYPE sytabix,
        var_valor        TYPE t16ft-frgxt,
        vg_maior         TYPE i,
        i                TYPE i.

  CLEAR: it_t16ft_aux.
  MOVE it_t16ft TO it_t16ft_aux.


  IF s_werks IS NOT INITIAL.

    it_ausp_2_aux[] = it_ausp_2[].
    it_ausp_4_aux[] = it_ausp_4[].

    "Exclui Centros com Centros de Custo
    LOOP AT it_ausp_3 ASSIGNING FIELD-SYMBOL(<fs_ausp3>).
      vg_sy_tabix = sy-tabix.
      READ TABLE it_ausp_2_aux WITH KEY objek = <fs_ausp3>-objek BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <fs_ausp3>-lkenz = 'X'.
        CONTINUE.
      ENDIF.

      wa_t16ft-frggr = <fs_ausp3>-objek(2).
      wa_t16ft-frgsx = <fs_ausp3>-objek+2(2).

      READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16ft-frggr
                                                 frgsx = wa_t16ft-frgsx BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        PERFORM retorna_posicao USING wa_t16fs CHANGING vg_maior.
        <fs_ausp3>-adzhl = vg_maior.
      ENDIF.
    ENDLOOP.

    DELETE it_ausp_3 WHERE lkenz = 'X'.

    "Exclui Centro que não tem o Tipo selecionado
    IF s_tipo IS NOT INITIAL.
      DELETE it_ausp_4_aux WHERE atwrt NOT IN  s_tipo.
      LOOP AT it_ausp_3 ASSIGNING <fs_ausp3>.
        vg_sy_tabix = sy-tabix.
        READ TABLE it_ausp_4_aux WITH KEY objek = <fs_ausp3>-objek BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <fs_ausp3>-lkenz = 'Y'.
          CONTINUE.
        ENDIF.
      ENDLOOP.
      DELETE it_ausp_3 WHERE lkenz NE 'Y'.
    ENDIF.

    "Pedidos
    LOOP AT it_t001w INTO wa_t001w.
      aux_atwrt = wa_t001w-werks.
      CLEAR: wa_saida.
      READ TABLE it_ausp_3 WITH KEY atwrt = aux_atwrt TRANSPORTING NO FIELDS.
      vg_subrc_ausp_02 = sy-subrc.
      IF vg_subrc_ausp_02 IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      wa_saida-werks = wa_t001w-werks.           "Centro
      wa_saida-name1 = wa_t001w-name1.           "Descrição
      APPEND wa_saida TO it_saida.
    ENDLOOP.

    LOOP AT it_t001w INTO wa_t001w.
      aux_atwrt = wa_t001w-werks.
      i = 1.
      WHILE i LE 8.
        LOOP AT it_ausp_3 ASSIGNING <fs_ausp3> WHERE atwrt = aux_atwrt AND adzhl EQ i .
          wa_t16fs-frggr = <fs_ausp3>-objek(2).
          wa_t16fs-frgsx = <fs_ausp3>-objek+2(2).
          "(2)
          PERFORM busca_valor_nome_pedido_centro USING wa_t16fs-frggr wa_t16fs-frgsx aux_atwrt CHANGING wa_ped.
          IF wa_ped IS NOT INITIAL.
            "(1)PEDIDO
            READ TABLE it_saida INTO wa_saida WITH KEY werks = wa_t001w-werks.
            IF sy-subrc IS INITIAL.
              vg_sy_tabix = sy-tabix.
              IF wa_saida-pedido_nome_01 IS INITIAL.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                MODIFY it_saida INDEX sy-tabix FROM wa_saida.

                IF wa_saida-pedido_valor_01 IS INITIAL.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  MODIFY it_saida INDEX sy-tabix FROM wa_saida.
                ENDIF.
              ELSE.
                MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
                CLEAR: wa_saida.
                wa_saida-werks           = wa_saida_aux-werks.
                wa_saida-name1           = wa_saida_aux-name1.
                wa_saida-objek           = wa_saida_aux-objek.
                wa_saida-atwrt           = wa_saida_aux-atwrt.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                INSERT wa_saida INTO it_saida INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        ADD 1 TO i.
      ENDWHILE.
    ENDLOOP.

  ELSE.

    LOOP AT it_ausp_2 ASSIGNING FIELD-SYMBOL(<fs_ausp_2>).
      wa_t16ft-frggr = <fs_ausp_2>-objek(2).
      wa_t16ft-frgsx = <fs_ausp_2>-objek+2(2).
      vg_sy_tabix = sy-tabix.
      READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16ft-frggr
                                                 frgsx = wa_t16ft-frgsx BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        PERFORM retorna_posicao USING wa_t16fs CHANGING vg_maior.
        <fs_ausp_2>-adzhl = vg_maior.
      ENDIF.
    ENDLOOP.

    LOOP AT it_csks INTO wa_csks.

      aux_atwrt = wa_csks-kostl.

      CLEAR: wa_saida.

      READ TABLE it_ausp WITH KEY atwrt = aux_atwrt TRANSPORTING NO FIELDS.
      vg_subrc_ausp_01 = sy-subrc.

      READ TABLE it_ausp_2 WITH KEY atwrt = aux_atwrt TRANSPORTING NO FIELDS.
      vg_subrc_ausp_02 = sy-subrc.

      IF vg_subrc_ausp_02 IS NOT INITIAL AND vg_subrc_ausp_01 IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      "CENTRO DE CUSTO E DESCRIÇÃO
      READ TABLE it_cskt INTO wa_cskt WITH KEY kokrs = wa_csks-kokrs
                                               kostl = wa_csks-kostl
                                               datbi = wa_csks-datbi.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      wa_saida-kostl = wa_csks-kostl.           "Centro de custo
      wa_saida-ktext = wa_cskt-ktext.           "Descrição
      wa_saida-bukrs = wa_csks-bukrs.           "Empresa
      wa_saida-gsber = wa_csks-gsber.           "Centro
      IF wa_csks-bkzkp = 'X'.
        wa_saida-bloq  = 'X'.
      ELSE.
        CLEAR  wa_saida-bloq.
      ENDIF.
      "COD. CLIENTE.
      LOOP AT it_zmmt0003 INTO wa_zmmt0003 WHERE kostl = wa_csks-kostl AND uname NE ''.
        wa_saida-uname = wa_zmmt0003-uname.

        "Reservas
        READ TABLE it_user_addr INTO wa_user_addr WITH KEY bname = wa_zmmt0003-uname.
        IF sy-subrc IS INITIAL.
          wa_saida-name_textc = wa_user_addr-name_textc.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ENDLOOP.


      LOOP AT it_t001 INTO wa_t001 WHERE bukrs = wa_csks-bukrs.
        wa_saida-butxt =  wa_t001-butxt.
      ENDLOOP.

      LOOP AT it_t001w INTO wa_t001w WHERE werks = wa_csks-gsber.
        wa_saida-name1 =  wa_t001w-name1.
      ENDLOOP.


      IF wa_saida-name_textc IS INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ENDLOOP.


    "Requisição
    LOOP AT it_csks INTO wa_csks.
      aux_atwrt = wa_csks-kostl.
      LOOP AT it_ausp WHERE atwrt = aux_atwrt.
        wa_t16ft-frggr = it_ausp-objek(2).
        wa_t16ft-frgsx = it_ausp-objek+2(2).
        READ TABLE it_t16ft INTO wa_t16ft
                            WITH KEY frggr = wa_t16ft-frggr
                                     frgsx = wa_t16ft-frgsx BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_saida INTO wa_saida WITH KEY kostl = wa_csks-kostl.
          vg_sy_tabix = sy-tabix.
          IF ( wa_saida-requisicao_nome IS INITIAL ) OR ( wa_saida-requisicao_nome = wa_t16ft-frgxt ).
            IF wa_saida-requisicao_nome NE wa_t16ft-frgxt.
              wa_saida-requisicao_nome = wa_t16ft-frgxt.
              MODIFY it_saida INDEX sy-tabix FROM wa_saida TRANSPORTING requisicao_nome.
            ENDIF.
          ELSE.
            wa_saida-requisicao_nome = wa_t16ft-frgxt.
            ADD 1 TO vg_sy_tabix.
            INSERT wa_saida INTO it_saida INDEX vg_sy_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_csks INTO wa_csks.
      aux_atwrt = wa_csks-kostl.
      i = 1.
      WHILE i LE 8.
        LOOP AT it_ausp_2 ASSIGNING <fs_ausp_2> WHERE atwrt = aux_atwrt AND adzhl EQ i.

          wa_t16fs-frggr = <fs_ausp_2>-objek(2).
          wa_t16fs-frgsx = <fs_ausp_2>-objek+2(2).
          "(2)
          PERFORM busca_valor_nome_pedido USING wa_t16fs-frggr wa_t16fs-frgsx aux_atwrt CHANGING wa_ped.
          IF wa_ped IS NOT INITIAL.
            "(1)PEDIDO
            READ TABLE it_saida INTO wa_saida WITH KEY kostl = wa_csks-kostl.
            IF sy-subrc IS INITIAL.
              vg_sy_tabix = sy-tabix.
              IF wa_saida-pedido_nome_01 IS INITIAL.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                MODIFY it_saida INDEX sy-tabix FROM wa_saida.

                IF wa_saida-pedido_valor_01 IS INITIAL.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  MODIFY it_saida INDEX sy-tabix FROM wa_saida.
                ENDIF.
              ELSE.
                MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
                CLEAR: wa_saida.
                wa_saida-uname           = wa_saida_aux-uname.
                wa_saida-kostl           = wa_saida_aux-kostl.
                wa_saida-ktext           = wa_saida_aux-ktext.
                wa_saida-objek           = wa_saida_aux-objek.
                wa_saida-atwrt           = wa_saida_aux-atwrt.
                wa_saida-requisicao_nome = wa_saida_aux-requisicao_nome.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                INSERT wa_saida INTO it_saida INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        ADD 1 TO i.
      ENDWHILE.
    ENDLOOP.
  ENDIF.

ENDFORM.

FORM busca_valor_nome_pedido  USING    p_frggr TYPE frggr
                                       p_frgsx TYPE frgsx
                                       p_csks  TYPE atwrt
                              CHANGING out_ped TYPE ty_pedido.

  CLEAR: out_ped.

  READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    "Valores/Nomes da Estratégia
    "Valor/nome 1

    IF wa_t16fs-frgc1 IS NOT INITIAL.
      wa_ausp_2-adzhl = 1.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc1
                                 CHANGING out_ped-pedido_valor_01 out_ped-pedido_nome_01.
    ENDIF.

    "Valor/nome 2
    IF wa_t16fs-frgc2 IS NOT INITIAL.
      wa_ausp_2-adzhl = 2.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc2
                                 CHANGING out_ped-pedido_valor_02 out_ped-pedido_nome_02.
    ENDIF.

    "Valor/nome 3
    IF wa_t16fs-frgc3 IS NOT INITIAL.
      wa_ausp_2-adzhl = 3.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc3
                                 CHANGING out_ped-pedido_valor_03 out_ped-pedido_nome_03.
    ENDIF.

    "Valor/nome 4
    IF wa_t16fs-frgc4 IS NOT INITIAL.
      wa_ausp_2-adzhl = 4.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc4
                                 CHANGING out_ped-pedido_valor_04 out_ped-pedido_nome_04.
    ENDIF.

    "Valor/nome 5
    IF wa_t16fs-frgc5 IS NOT INITIAL.
      wa_ausp_2-adzhl = 5.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc5
                                 CHANGING out_ped-pedido_valor_05 out_ped-pedido_nome_05.
    ENDIF.

    "Valor/nome 6
    IF wa_t16fs-frgc6 IS NOT INITIAL.
      wa_ausp_2-adzhl = 6.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc6
                                 CHANGING out_ped-pedido_valor_06 out_ped-pedido_nome_06.
    ENDIF.

    "Valor/nome 7
    IF wa_t16fs-frgc7 IS NOT INITIAL.
      wa_ausp_2-adzhl = 7.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc7
                                 CHANGING out_ped-pedido_valor_07 out_ped-pedido_nome_07.
    ENDIF.

    "Valor/nome 8
    IF wa_t16fs-frgc8 IS NOT INITIAL.
      wa_ausp_2-adzhl = 8.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc8
                                 CHANGING out_ped-pedido_valor_08 out_ped-pedido_nome_08.
    ENDIF.
  ENDIF.

ENDFORM.

FORM busca_valor_sequencia  USING    p_csks    TYPE atwrt
                                     p_adzhl   TYPE adzhl
                                     p_frggr   TYPE frggr
                                     p_frgcx   TYPE frgsx
                            CHANGING p_valor_x TYPE t16ft-frgxt
                                     p_nome_x  TYPE t16fd-frgct.

  READ TABLE it_ausp_2 INTO wa_ausp_2 WITH KEY atwrt = p_csks
                                               adzhl = p_adzhl.
  IF sy-subrc IS INITIAL.
    READ TABLE it_t16ft INTO wa_t16ft WITH KEY frggr = wa_ausp_2-objek(2)
                                               frgsx = wa_ausp_2-objek+2(2) BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      p_valor_x = wa_t16ft-frgxt.
    ENDIF.

    "NOMES PEDIDOS.
    READ TABLE it_t16fd INTO wa_t16fd WITH KEY frggr = p_frggr
                                               frgco = p_frgcx BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      p_nome_x = wa_t16fd-frgct.
    ENDIF.
  ENDIF.

ENDFORM.

FORM busca_valor_nome_pedido_centro  USING    p_frggr TYPE frggr
                                       p_frgsx TYPE frgsx
                                       p_csks  TYPE atwrt
                              CHANGING out_ped TYPE ty_pedido.

  CLEAR: out_ped.

  READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    "Valores/Nomes da Estratégia
    "Valor/nome 1

    IF wa_t16fs-frgc1 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc1 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_01 out_ped-pedido_nome_01.
    ENDIF.

    "Valor/nome 2
    IF wa_t16fs-frgc2 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc2 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_02 out_ped-pedido_nome_02.
    ENDIF.

    "Valor/nome 3
    IF wa_t16fs-frgc3 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc3 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_03 out_ped-pedido_nome_03.
    ENDIF.

    "Valor/nome 4
    IF wa_t16fs-frgc4 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc4 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_04 out_ped-pedido_nome_04.
    ENDIF.

    "Valor/nome 5
    IF wa_t16fs-frgc5 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc5 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_05 out_ped-pedido_nome_05.
    ENDIF.

    "Valor/nome 6
    IF wa_t16fs-frgc6 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc6 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_06 out_ped-pedido_nome_06.
    ENDIF.

    "Valor/nome 7
    IF wa_t16fs-frgc7 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc7 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_07 out_ped-pedido_nome_07.
    ENDIF.

    "Valor/nome 8
    IF wa_t16fs-frgc8 IS NOT INITIAL.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc8 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_08 out_ped-pedido_nome_08.
    ENDIF.
  ENDIF.

ENDFORM.

FORM busca_valor_sequencia_centro  USING    p_frgsx   TYPE frgsx
                                            p_frggr   TYPE frggr
                                            p_frgcx   TYPE frgsx
                                   CHANGING p_valor_x TYPE t16ft-frgxt
                                            p_nome_x  TYPE t16fd-frgct.


  READ TABLE it_t16ft INTO wa_t16ft WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH .
  IF sy-subrc IS INITIAL.
    p_valor_x = wa_t16ft-frgxt.
  ENDIF.

  "NOMES PEDIDOS.
  READ TABLE it_t16fd INTO wa_t16fd WITH KEY frggr = p_frggr
                                             frgco = p_frgcx BINARY SEARCH .
  IF sy-subrc IS INITIAL.
    p_nome_x = wa_t16fd-frgct.
  ENDIF.

ENDFORM.

FORM retorna_posicao  USING    p_t16fs   TYPE t16fs
                      CHANGING p_posicao TYPE i.

  IF p_t16fs-frgc1 IS NOT INITIAL.
    p_posicao = 1.
  ENDIF.

  IF p_t16fs-frgc2 IS NOT INITIAL.
    p_posicao = 2.
  ENDIF.

  IF p_t16fs-frgc3 IS NOT INITIAL.
    p_posicao = 3.
  ENDIF.

  IF p_t16fs-frgc4 IS NOT INITIAL.
    p_posicao = 4.
  ENDIF.

  IF p_t16fs-frgc5 IS NOT INITIAL.
    p_posicao = 5.
  ENDIF.

  IF p_t16fs-frgc6 IS NOT INITIAL.
    p_posicao = 6.
  ENDIF.

  IF p_t16fs-frgc7 IS NOT INITIAL.
    p_posicao = 7.
  ENDIF.

  IF p_t16fs-frgc8 IS NOT INITIAL.
    p_posicao = 8.
  ENDIF.

ENDFORM.
