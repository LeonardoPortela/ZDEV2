FUNCTION z_pfe_leitura_arq_aux.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(DIRETORIO) TYPE  ADMI_PATH OPTIONAL
*"     VALUE(ARQUIVO) TYPE  ADMI_PATH
*"     VALUE(UNIX) TYPE  CFLAG DEFAULT 'X'
*"     VALUE(LOCAL) TYPE  CFLAG OPTIONAL
*"     VALUE(GERAR_LOTES) TYPE  CFLAG DEFAULT 'X'
*"  EXCEPTIONS
*"      INVALID_EPS_SUBDIR
*"      SAPGPARAM_FAILED
*"      BUILD_DIRECTORY_FAILED
*"      NO_AUTHORIZATION
*"      READ_DIRECTORY_FAILED
*"      TOO_MANY_READ_ERRORS
*"      EMPTY_DIRECTORY_LIST
*"      NAO_ADMINISTRADORA
*"      NAO_LOCAL_NEGOCIO
*"      INTERVAL_NOT_FOUND
*"      NUMBER_RANGE_NOT_INTERN
*"      OUTROS_ERROS
*"----------------------------------------------------------------------

  TYPES BEGIN OF ty_valores.
  TYPES: cd_ciot   TYPE zciot,
         docnum    TYPE j_1bdocnum,
         vl_perda	 TYPE kwert,
         vl_quebra TYPE kwert.
  TYPES END OF ty_valores.

  DATA: arq                TYPE epsfilnam,
        it_dir             TYPE TABLE OF epsfili,
        wa_dir             TYPE epsfili,
        dir_arq            TYPE string,
        t_arquivo	         TYPE TABLE OF zpfe_arquivo WITH HEADER LINE,
        t_reg_cabecalho    TYPE TABLE OF zpfe_lote WITH HEADER LINE,
        t_reg_itens        TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        t_reg_itens_ad     TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        t_reg_itens_aux    TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        wa_reg_cabecalho   TYPE zpfe_lote,
        wa_reg_itens       TYPE zpfe_lote_item,
        wa_reg_itens_aux   TYPE zpfe_lote_item,
        tg_reg_itens_aux   TYPE TABLE OF zpfe_lote_item,
        it_file_table      TYPE TABLE OF sdokpath WITH HEADER LINE,
        it_dir_table       TYPE TABLE OF sdokpath WITH HEADER LINE,
        wa_file_table      TYPE sdokpath,
        dir_name           LIKE epsf-epsdirnam,
        file_mask          LIKE epsf-epsfilnam,
        st_lote            TYPE zpfe_numero_lote,
        st_lote_aux        TYPE zpfe_numero_lote,
        vg_lote            TYPE i,
        p_tipcontabil      TYPE ztipcontabil,
        wa_zcte_ciot       TYPE zcte_ciot,
        tg_zcte_ciot       TYPE TABLE OF zcte_ciot,
        wa_zcte_identifica TYPE zcte_identifica,
        tg_zcte_identifica TYPE TABLE OF zcte_identifica,
        vg_diferenca       TYPE j_1bnetqty,
        vg_toleravel       TYPE j_1bnetqty,
        it_zlest0025       TYPE TABLE OF zlest0025 WITH HEADER LINE,
        it_valores         TYPE TABLE OF ty_valores WITH HEADER LINE,
        vg_tabix           TYPE sy-tabix,
        vg_nm_lote_item	   TYPE zpfe_numero_lote,
        wl_tabix           TYPE sy-tabix.

  DATA: wdir TYPE c LENGTH 100,
        warq TYPE c LENGTH 100.


  CONCATENATE diretorio arquivo INTO arq.

  vg_lote = 0.

  IF unix EQ 'X'.

    dir_name  = diretorio.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = dir_name
      TABLES
        dir_list               = it_dir
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    CASE sy-subrc.
      WHEN 01.
        RAISE invalid_eps_subdir.
      WHEN 02.
        RAISE sapgparam_failed.
      WHEN 03.
        RAISE build_directory_failed.
      WHEN 04.
        RAISE no_authorization.
      WHEN 05.
        RAISE read_directory_failed.
      WHEN 06.
        RAISE too_many_read_errors.
      WHEN 07.
        MESSAGE e001 WITH diretorio RAISING empty_directory_list.
      WHEN 08.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
    ENDCASE.

    LOOP AT it_dir INTO wa_dir.

      CONCATENATE diretorio wa_dir-name INTO dir_arq.

      CALL FUNCTION 'Z_PFE_ARQUIVO'
        EXPORTING
          arquivo                 = dir_arq
        TABLES
          t_arquivo               = t_arquivo
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          outros                  = 17
          OTHERS                  = 18.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDIF.

      vg_lote = vg_lote + 1.

      CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
        TABLES
          t_arquivo          = t_arquivo
          t_reg_cabecalho    = t_reg_cabecalho
        CHANGING
          vg_lote            = vg_lote
        EXCEPTIONS
          nao_administradora = 1
          nao_local_negocio  = 2
          OTHERS             = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e002 WITH sy-msgv1 RAISING nao_administradora.
        WHEN 2.
          MESSAGE e003 WITH sy-msgv1 RAISING nao_local_negocio.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDCASE.

    ENDLOOP.

  ELSE.

    MOVE: diretorio TO wdir,
          arquivo   TO warq.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        directory  = wdir
        filter     = warq
      TABLES
        file_table = it_file_table
        dir_table  = it_dir_table
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
    ENDIF.

    LOOP AT it_file_table INTO wa_file_table.

      CONCATENATE diretorio wa_file_table-pathname INTO dir_arq.

      CALL FUNCTION 'Z_PFE_ARQUIVO'
        EXPORTING
          arquivo                 = dir_arq
          unix                    = space
          local                   = 'X'
        TABLES
          t_arquivo               = t_arquivo
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          outros                  = 17
          OTHERS                  = 18.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDIF.

      vg_lote = vg_lote + 1.

      CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
        TABLES
          t_arquivo          = t_arquivo
          t_reg_cabecalho    = t_reg_cabecalho
          t_reg_itens        = t_reg_itens
        CHANGING
          vg_lote            = vg_lote
        EXCEPTIONS
          nao_administradora = 1
          nao_local_negocio  = 2
          OTHERS             = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e002 WITH sy-msgv1 RAISING nao_administradora.
        WHEN 2.
          MESSAGE e003 WITH sy-msgv1 RAISING nao_local_negocio.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDCASE.

    ENDLOOP.

  ENDIF.

  IF gerar_lotes EQ 'X'.

    IF NOT t_reg_itens[] IS INITIAL.
      SELECT * INTO TABLE it_zlest0025
        FROM zlest0025.
    ENDIF.

    MOVE t_reg_itens[] TO t_reg_itens_ad[].
    DELETE t_reg_itens    WHERE chvid EQ '1'.
    DELETE t_reg_itens_ad WHERE chvid NE '1'.
    MOVE t_reg_itens[] TO t_reg_itens_aux[].

    "Adiantamento
    vg_nm_lote_item = 1.
    LOOP AT t_reg_cabecalho INTO wa_reg_cabecalho.
      READ TABLE t_arquivo INDEX 1.
      CLEAR: p_tipcontabil.

      READ TABLE t_reg_itens_ad WITH KEY nm_lote = wa_reg_cabecalho-nm_lote.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      wa_reg_cabecalho-vl_total_lote = 0.
      wa_reg_cabecalho-vl_confi_lote = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
        EXPORTING
          p_dt_posicao  = wa_reg_cabecalho-dt_posicao
        IMPORTING
          p_tipcontabil = p_tipcontabil.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZPFELOTE'
        IMPORTING
          number                  = st_lote
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          OTHERS                  = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e004 WITH 'ZPFELOTE' RAISING interval_not_found.
        WHEN 2.
          MESSAGE e005 WITH '01' 'ZPFELOTE' RAISING number_range_not_intern.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDCASE.

      st_lote_aux = wa_reg_cabecalho-nm_lote.

      wa_reg_cabecalho-status  = 'I'.
      wa_reg_cabecalho-nm_lote = st_lote.
      IF t_reg_itens_ad[] IS NOT INITIAL.
        SELECT *
          FROM zpfe_lote_item
          INTO TABLE tg_reg_itens_aux
           FOR ALL ENTRIES IN t_reg_itens_ad
           WHERE nucontrato EQ t_reg_itens_ad-nucontrato
             AND chvid      EQ t_reg_itens_ad-chvid.

        SELECT  *
          FROM zcte_ciot
          INTO TABLE tg_zcte_ciot
           FOR ALL ENTRIES IN t_reg_itens_ad
           WHERE nucontrato EQ t_reg_itens_ad-nucontrato.

        IF sy-subrc IS INITIAL.
          SELECT  *
            FROM zcte_identifica
            INTO TABLE tg_zcte_identifica
            FOR ALL ENTRIES IN tg_zcte_ciot
             WHERE docnum EQ tg_zcte_ciot-docnum.
        ENDIF.
        SORT: tg_reg_itens_aux BY nucontrato chvid.
        SORT: tg_zcte_ciot BY nucontrato.
        SORT: tg_zcte_identifica BY docnum.
      ENDIF.

      LOOP AT t_reg_itens_ad INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux.
        wl_tabix = sy-tabix.
        ADD 1 TO wl_tabix.
        READ TABLE t_arquivo INDEX wl_tabix.
*        SELECT SINGLE * INTO WA_REG_ITENS_AUX
*          FROM ZPFE_LOTE_ITEM
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
*           AND CHVID      EQ WA_REG_ITENS-CHVID.
        READ TABLE tg_reg_itens_aux INTO wa_reg_itens_aux
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                   chvid      = wa_reg_itens-chvid
                        BINARY SEARCH.


        IF sy-subrc IS INITIAL.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                  USING wa_reg_cabecalho-nr_lote_adm
                                        space
                                        wa_reg_itens-chvid
                                        wa_reg_itens-nucontrato
                                        'F'
                                        text-e03
                                        t_arquivo-linha
                                        'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

*        SELECT SINGLE * INTO WA_ZCTE_CIOT
*          FROM ZCTE_CIOT
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.
        READ TABLE tg_zcte_ciot INTO wa_zcte_ciot
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                        BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

          IF wa_reg_itens-vl_transacao NE wa_zcte_ciot-vlr_adiantamento.
            PERFORM insert_line_log IN PROGRAM sapmzles003
                                  USING wa_reg_cabecalho-nr_lote_adm
                                        space
                                        wa_reg_itens-chvid
                                        wa_reg_itens-nucontrato
                                        'F'
                                        text-e04
                                        t_arquivo-linha
                                        'E'.
            "Erro Adiantamento
            CONTINUE.
          ENDIF.

*          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
*            FROM ZCTE_IDENTIFICA
*           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.
          READ TABLE tg_zcte_identifica  INTO wa_zcte_identifica
            WITH KEY docnum = wa_zcte_ciot-docnum
                      BINARY SEARCH.

          wa_reg_itens-cd_ciot    = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot    = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum	    = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum      = wa_zcte_ciot-tknum.
          wa_reg_itens-ctenum     = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie   = wa_zcte_identifica-serie.
          wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
          wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
          wa_reg_itens-ck_conferido    = ''.
          wa_reg_itens-ds_usuario_conf = sy-uname.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_transacao.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_reg_itens-nm_lote_item
            IMPORTING
              output = wa_reg_itens-nm_lote_item.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_itens-nr_lote_adm
                                       wa_reg_itens-nm_lote
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-s01
                                       space
                                       'S'.
        ELSE.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                  USING wa_reg_cabecalho-nr_lote_adm
                                        space
                                        wa_reg_itens-chvid
                                        wa_reg_itens-nucontrato
                                        'F'
                                        text-e05
                                        t_arquivo-linha
                                        'E'.
          "Erro de Contrato não encontrado

        ENDIF.
      ENDLOOP.

      wa_reg_cabecalho-tplote = 'A'.

      MODIFY zpfe_lote FROM wa_reg_cabecalho.
      PERFORM insert_line_log IN PROGRAM sapmzles003
                               USING wa_reg_cabecalho-nr_lote_adm
                                     wa_reg_cabecalho-nm_lote
                                     space
                                     space
                                     'F'
                                     text-s02
                                     space
                                     'S'.
    ENDLOOP.

    IF t_reg_itens[] IS NOT INITIAL.
      SELECT  *
        FROM zpfe_lote_item
        INTO TABLE tg_reg_itens_aux
        FOR ALL ENTRIES IN t_reg_itens
         WHERE nucontrato EQ t_reg_itens-nucontrato
           AND chvid      EQ t_reg_itens-chvid.

      SELECT *
        FROM zcte_ciot
        INTO TABLE tg_zcte_ciot
        FOR ALL ENTRIES IN t_reg_itens
         WHERE nucontrato EQ t_reg_itens-nucontrato.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zcte_identifica
          INTO TABLE tg_zcte_identifica
          FOR ALL ENTRIES IN tg_zcte_ciot
         WHERE docnum EQ tg_zcte_ciot-docnum.
      ENDIF.

      SORT: tg_reg_itens_aux BY nucontrato chvid.
      SORT: tg_zcte_ciot BY nucontrato.
      SORT: tg_zcte_identifica BY docnum.
    ENDIF.

    "Resto de Chaves
    vg_nm_lote_item = 1.
    LOOP AT t_reg_cabecalho INTO wa_reg_cabecalho.

      CLEAR: p_tipcontabil.

      READ TABLE t_reg_itens WITH KEY nm_lote = wa_reg_cabecalho-nm_lote.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      wa_reg_cabecalho-vl_total_lote = 0.
      wa_reg_cabecalho-vl_confi_lote = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
        EXPORTING
          p_dt_posicao  = wa_reg_cabecalho-dt_posicao
        IMPORTING
          p_tipcontabil = p_tipcontabil.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZPFELOTE'
        IMPORTING
          number                  = st_lote
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          OTHERS                  = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e004 WITH 'ZPFELOTE' RAISING interval_not_found.
        WHEN 2.
          MESSAGE e005 WITH '01' 'ZPFELOTE' RAISING number_range_not_intern.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros_erros.
      ENDCASE.

      st_lote_aux = wa_reg_cabecalho-nm_lote.

      wa_reg_cabecalho-status  = 'I'.
      wa_reg_cabecalho-nm_lote = st_lote.

      "Verificações de Saldo de Frete
      LOOP AT t_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid EQ '2'.
        wl_tabix = sy-tabix.
        ADD 1 TO wl_tabix.
        READ TABLE t_arquivo INDEX wl_tabix.
        READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid.

        IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).


          CONTINUE.
        ENDIF.

*        SELECT SINGLE * INTO WA_REG_ITENS_AUX
*          FROM ZPFE_LOTE_ITEM
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
*           AND CHVID      EQ WA_REG_ITENS-CHVID.
        READ TABLE tg_reg_itens_aux INTO wa_reg_itens_aux
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                   chvid      = wa_reg_itens-chvid
                        BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                  USING wa_reg_cabecalho-nr_lote_adm
                                        space
                                        wa_reg_itens-chvid
                                        wa_reg_itens-nucontrato
                                        'F'
                                        text-e03
                                        t_arquivo-linha
                                        'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

*        SELECT SINGLE * INTO WA_ZCTE_CIOT
*          FROM ZCTE_CIOT
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.
        READ TABLE tg_zcte_ciot INTO wa_zcte_ciot
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                        BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

*          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
*            FROM ZCTE_IDENTIFICA
*           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.

          READ TABLE tg_zcte_identifica INTO wa_zcte_identifica
            WITH KEY docnum = wa_zcte_ciot-docnum
                      BINARY SEARCH.

          it_valores-cd_ciot   = wa_zcte_ciot-cd_ciot.
          it_valores-docnum    = wa_zcte_ciot-docnum.
          it_valores-vl_quebra = 0.
          it_valores-vl_perda  = 0.

          wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum	     = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
          wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
          wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
          wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          IF p_tipcontabil EQ 'FC'.
            wa_reg_itens-peso_chegada = wa_reg_itens-peso_importado.

            "if wa_reg_itens-chvid eq '2'.
*            WA_ZCTE_CIOT-VLR_FRETE = ( ( WA_ZCTE_CIOT-VLR_FRETE -
*                                     WA_ZCTE_CIOT-VLR_ADIANTAMENTO -
**                                     wa_zcte_ciot-vlr_seguro -
*                                     WA_ZCTE_CIOT-VLR_IMPOSTOS ) + WA_ZCTE_CIOT-VLR_IOF ).

            wa_zcte_ciot-vlr_frete = wa_zcte_ciot-vlr_frete -
                                     wa_zcte_ciot-vlr_adiantamento -
                                     wa_zcte_ciot-vlr_seguro -
                                     wa_zcte_ciot-vlr_impostos.

            IF wa_reg_itens-peso_chegada GT wa_reg_itens-peso_origem.
              wa_reg_itens-peso_chegada = wa_reg_itens-peso_origem.
            ENDIF.
            wa_reg_itens-vl_pago_lote = wa_zcte_ciot-vlr_frete.

            IF ( wa_reg_itens-peso_chegada LT wa_reg_itens-peso_origem ) OR
               ( wa_reg_itens-vl_transacao GT wa_zcte_ciot-vlr_frete ).
              vg_diferenca = wa_reg_itens-peso_origem - wa_reg_itens-peso_chegada.

              "Valor de Quebra
              it_valores-vl_quebra  = ( vg_diferenca * ( wa_zcte_ciot-vlr_unit_frete / 1000 ) ).
              "Valor da Perda
              vg_toleravel          = wa_reg_itens-peso_origem * ( wa_zcte_ciot-perc_tolerancia / 100 ).
              IF vg_diferenca GT vg_toleravel.
                it_valores-vl_perda = ( ( vg_diferenca - vg_toleravel ) * wa_zcte_ciot-vlr_unit_merc  ).
              ENDIF.
            ENDIF.
            "else.
            "  wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_transacao.
            "endif.
          ELSEIF  p_tipcontabil EQ 'FS'.
            wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
            wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
            wa_reg_itens-peso_chegada    = wa_reg_itens-peso_importado.
            wa_reg_itens-ck_conferido    = 'X'.
            wa_reg_itens-ds_usuario_conf = sy-uname.
          ENDIF.
          APPEND it_valores.
          IF it_zlest0025-naturezachvid EQ 'S'.
            wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_pago_lote * ( -1 ).
          ENDIF.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_reg_itens-nm_lote_item
            IMPORTING
              output = wa_reg_itens-nm_lote_item.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_itens-nr_lote_adm
                                       wa_reg_itens-nm_lote
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-s01
                                       space
                                       'S'.
        ELSE.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_cabecalho-nr_lote_adm
                                       space
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-e05
                                       t_arquivo-linha
                                       'E'.
          "Erro de Contrato não encontrado
        ENDIF.
      ENDLOOP.

      LOOP AT t_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid NE '2'.
        wl_tabix = sy-tabix.
        wl_tabix = ( wl_tabix + 1 ).
        ADD 1 TO wl_tabix.
        READ TABLE t_arquivo INDEX wl_tabix.
        READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid.

        IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                            USING wa_reg_cabecalho-nr_lote_adm
                                                  space
                                                  wa_reg_itens-chvid
                                                  wa_reg_itens-nucontrato
                                                  'F'
                                                  text-e06
                                                  t_arquivo-linha
                                                  'E'.
          CONTINUE.
        ENDIF.

*        SELECT SINGLE * INTO WA_REG_ITENS_AUX
*          FROM ZPFE_LOTE_ITEM
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
*           AND CHVID      EQ WA_REG_ITENS-CHVID.
        READ TABLE tg_reg_itens_aux INTO wa_reg_itens_aux
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                   chvid      = wa_reg_itens-chvid
                        BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_cabecalho-nr_lote_adm
                                       space
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-e03
                                       t_arquivo-linha
                                       'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

*        SELECT SINGLE * INTO WA_ZCTE_CIOT
*          FROM ZCTE_CIOT
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.
        READ TABLE tg_zcte_ciot INTO wa_zcte_ciot
          WITH KEY nucontrato = wa_reg_itens-nucontrato
                        BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

*          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
*            FROM ZCTE_IDENTIFICA
*           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.
          READ TABLE tg_zcte_identifica INTO wa_zcte_identifica
            WITH KEY docnum = wa_zcte_ciot-docnum
                      BINARY SEARCH.

          wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum	     = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
          wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
          wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
          IF it_zlest0025-naturezachvid EQ 'S'.
            wa_reg_itens-vl_transacao = wa_reg_itens-vl_transacao * ( -1 ).
          ENDIF.
          wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          IF p_tipcontabil EQ 'FC'.
            wa_reg_itens-vl_diferenca  = 0.

            CASE wa_reg_itens-chvid.
              WHEN '30'.
                READ TABLE it_valores WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                                               docnum  = wa_zcte_ciot-docnum.
                IF sy-subrc IS INITIAL.
                  vg_tabix = sy-tabix.
                  wa_reg_itens-vl_pago_lote = it_valores-vl_quebra * -1.
                  it_valores-vl_quebra      = 0.
                  MODIFY it_valores INDEX vg_tabix TRANSPORTING vl_quebra.
                ENDIF.
                "Quebra
              WHEN '31'.
                "Perda
                READ TABLE it_valores WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                                               docnum  = wa_zcte_ciot-docnum.
                IF sy-subrc IS INITIAL.
                  vg_tabix = sy-tabix.
                  wa_reg_itens-vl_pago_lote = it_valores-vl_perda * -1.
                  it_valores-vl_perda       = 0.
                  MODIFY it_valores INDEX vg_tabix TRANSPORTING vl_perda.
                ENDIF.
              WHEN OTHERS.
                wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_transacao.
            ENDCASE.

          ELSEIF  p_tipcontabil EQ 'FS'.
            wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
            wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
            wa_reg_itens-ck_conferido    = 'X'.
            wa_reg_itens-ds_usuario_conf = sy-uname.
          ENDIF.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_reg_itens-nm_lote_item
            IMPORTING
              output = wa_reg_itens-nm_lote_item.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_itens-nr_lote_adm
                                       wa_reg_itens-nm_lote
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-s01
                                       space
                                       'S'.
        ELSE.
          PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_cabecalho-nr_lote_adm
                                       space
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-e05
                                       t_arquivo-linha
                                       'E'.
          "Erro de Contrato não encontrado
        ENDIF.
      ENDLOOP.

      IF p_tipcontabil EQ 'FC'.
        IF it_valores[] IS NOT INITIAL.
          SELECT *
            FROM zcte_ciot
            INTO TABLE tg_zcte_ciot
             FOR ALL ENTRIES IN it_valores
             WHERE cd_ciot EQ it_valores-cd_ciot.

          SELECT *
            FROM zcte_identifica
            INTO TABLE tg_zcte_identifica
            FOR ALL ENTRIES IN it_valores
             WHERE docnum EQ it_valores-docnum.

          SORT tg_zcte_ciot BY cd_ciot.
          SORT tg_zcte_identifica BY docnum.
        ENDIF.
        LOOP AT it_valores WHERE ( vl_quebra GT 0 OR vl_perda GT 0 ).

          wa_reg_itens-nm_lote = st_lote.
          wa_reg_itens-status  = 'I'.

*          SELECT SINGLE * INTO WA_ZCTE_CIOT
*            FROM ZCTE_CIOT
*           WHERE CD_CIOT EQ IT_VALORES-CD_CIOT.
          READ TABLE tg_zcte_ciot INTO wa_zcte_ciot
            WITH KEY cd_ciot = it_valores-cd_ciot
                       BINARY SEARCH.

*          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
*            FROM ZCTE_IDENTIFICA
*           WHERE DOCNUM EQ IT_VALORES-DOCNUM.
          READ TABLE tg_zcte_identifica INTO wa_zcte_identifica
            WITH KEY docnum = it_valores-docnum
                      BINARY SEARCH.

          wa_reg_itens-cd_ciot       = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot       = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum	       = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum         = wa_zcte_ciot-tknum.
          wa_reg_itens-ctenum        = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie      = wa_zcte_identifica-serie.

          wa_reg_itens-vl_pago_lote  = 0.
          wa_reg_itens-vl_transacao  = 0.
          wa_reg_itens-vl_diferenca  = 0.

          IF it_valores-vl_quebra GT 0.
            READ TABLE it_zlest0025 WITH KEY chvid = '30'.
            wa_reg_itens-vl_pago_lote = it_valores-vl_quebra * ( -1 ).
            wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
            wa_reg_itens-chvid = '30'.
            wa_reg_itens-nm_lote_item = vg_nm_lote_item.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_reg_itens-nm_lote_item
              IMPORTING
                output = wa_reg_itens-nm_lote_item.

            MODIFY zpfe_lote_item FROM wa_reg_itens.
            vg_nm_lote_item = vg_nm_lote_item + 1.
          ENDIF.
          IF it_valores-vl_perda GT 0.
            READ TABLE it_zlest0025 WITH KEY chvid = '31'.
            wa_reg_itens-vl_pago_lote = it_valores-vl_perda * ( -1 ).
            wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
            wa_reg_itens-chvid = '31'.
            wa_reg_itens-nm_lote_item = vg_nm_lote_item.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_reg_itens-nm_lote_item
              IMPORTING
                output = wa_reg_itens-nm_lote_item.

            MODIFY zpfe_lote_item FROM wa_reg_itens.
            vg_nm_lote_item = vg_nm_lote_item + 1.
            PERFORM insert_line_log IN PROGRAM sapmzles003
                                 USING wa_reg_itens-nr_lote_adm
                                       wa_reg_itens-nm_lote
                                       wa_reg_itens-chvid
                                       wa_reg_itens-nucontrato
                                       'F'
                                       text-s01
                                       space
                                       'S'.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CLEAR: it_valores[].

      wa_reg_cabecalho-tplote = 'S'.
      MODIFY zpfe_lote FROM wa_reg_cabecalho.
      PERFORM insert_line_log IN PROGRAM sapmzles003
                                      USING wa_reg_cabecalho-nr_lote_adm
                                            wa_reg_cabecalho-nm_lote
                                            space
                                            space
                                            'F'
                                            text-s02
                                            space
                                            'S'.
    ENDLOOP.

    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
