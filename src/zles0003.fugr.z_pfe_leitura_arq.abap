FUNCTION Z_PFE_LEITURA_ARQ.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DIRETORIO) TYPE  CLIKE OPTIONAL
*"     REFERENCE(ARQUIVO) TYPE  CLIKE
*"     REFERENCE(UNIX) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(LOCAL) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(GERAR_LOTES) TYPE  CHAR01 DEFAULT 'X'
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

  TYPES BEGIN OF TY_VALORES.
  TYPES: CD_CIOT   TYPE ZCIOT,
         DOCNUM    TYPE J_1BDOCNUM,
         VL_PERDA	 TYPE KWERT,
         VL_QUEBRA TYPE KWERT.
  TYPES END OF TY_VALORES.

  DATA: ARQ                TYPE EPSFILNAM,
        IT_DIR             TYPE TABLE OF EPSFILI,
        WA_DIR             TYPE EPSFILI,
        DIR_ARQ            TYPE STRING,
        T_ARQUIVO	         TYPE TABLE OF ZPFE_ARQUIVO WITH HEADER LINE,
        T_REG_CABECALHO    TYPE TABLE OF ZPFE_LOTE WITH HEADER LINE,
        T_REG_ITENS        TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        T_REG_ITENS_AD     TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        T_REG_ITENS_AUX    TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        WA_REG_CABECALHO   TYPE ZPFE_LOTE,
        WA_REG_ITENS       TYPE ZPFE_LOTE_ITEM,
        WA_REG_ITENS_AUX   TYPE ZPFE_LOTE_ITEM,
        IT_FILE_TABLE      TYPE TABLE OF SDOKPATH WITH HEADER LINE,
        IT_DIR_TABLE       TYPE TABLE OF SDOKPATH WITH HEADER LINE,
        WA_FILE_TABLE      TYPE SDOKPATH,
        DIR_NAME           LIKE EPSF-EPSDIRNAM,
        FILE_MASK          LIKE EPSF-EPSFILNAM,
        ST_LOTE            TYPE ZPFE_NUMERO_LOTE,
        ST_LOTE_AUX        TYPE ZPFE_NUMERO_LOTE,
        VG_LOTE            TYPE I,
        P_TIPCONTABIL      TYPE ZTIPCONTABIL,
        WA_ZCTE_CIOT       TYPE ZCTE_CIOT,
        WA_ZCTE_IDENTIFICA TYPE ZCTE_IDENTIFICA,
        VG_DIFERENCA       TYPE J_1BNETQTY,
        VG_TOLERAVEL       TYPE J_1BNETQTY,
        IT_ZLEST0025       TYPE TABLE OF ZLEST0025 WITH HEADER LINE,
        IT_VALORES         TYPE TABLE OF TY_VALORES WITH HEADER LINE,
        VG_TABIX           TYPE SY-TABIX,
        VG_NM_LOTE_ITEM	   TYPE ZPFE_NUMERO_LOTE,
        WL_TABIX           TYPE SY-TABIX.

  DATA: WDIR TYPE C LENGTH 100,
        WARQ TYPE C LENGTH 100.

  CONCATENATE DIRETORIO ARQUIVO INTO ARQ.

  VG_LOTE = 0.

  IF UNIX EQ 'X'.

    DIR_NAME  = DIRETORIO.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        DIR_NAME               = DIR_NAME
      TABLES
        DIR_LIST               = IT_DIR
      EXCEPTIONS
        INVALID_EPS_SUBDIR     = 1
        SAPGPARAM_FAILED       = 2
        BUILD_DIRECTORY_FAILED = 3
        NO_AUTHORIZATION       = 4
        READ_DIRECTORY_FAILED  = 5
        TOO_MANY_READ_ERRORS   = 6
        EMPTY_DIRECTORY_LIST   = 7
        OTHERS                 = 8.

    CASE SY-SUBRC.
      WHEN 01.
        RAISE INVALID_EPS_SUBDIR.
      WHEN 02.
        RAISE SAPGPARAM_FAILED.
      WHEN 03.
        RAISE BUILD_DIRECTORY_FAILED.
      WHEN 04.
        RAISE NO_AUTHORIZATION.
      WHEN 05.
        RAISE READ_DIRECTORY_FAILED.
      WHEN 06.
        RAISE TOO_MANY_READ_ERRORS.
      WHEN 07.
        MESSAGE E001 WITH DIRETORIO RAISING EMPTY_DIRECTORY_LIST.
      WHEN 08.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
    ENDCASE.

    LOOP AT IT_DIR INTO WA_DIR.

      CONCATENATE DIRETORIO WA_DIR-NAME INTO DIR_ARQ.

      CALL FUNCTION 'Z_PFE_ARQUIVO'
        EXPORTING
          ARQUIVO                 = DIR_ARQ
        TABLES
          T_ARQUIVO               = T_ARQUIVO
        EXCEPTIONS
          FILE_OPEN_ERROR         = 1
          FILE_READ_ERROR         = 2
          NO_BATCH                = 3
          GUI_REFUSE_FILETRANSFER = 4
          INVALID_TYPE            = 5
          NO_AUTHORITY            = 6
          UNKNOWN_ERROR           = 7
          BAD_DATA_FORMAT         = 8
          HEADER_NOT_ALLOWED      = 9
          SEPARATOR_NOT_ALLOWED   = 10
          HEADER_TOO_LONG         = 11
          UNKNOWN_DP_ERROR        = 12
          ACCESS_DENIED           = 13
          DP_OUT_OF_MEMORY        = 14
          DISK_FULL               = 15
          DP_TIMEOUT              = 16
          OUTROS                  = 17
          OTHERS                  = 18.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDIF.

      VG_LOTE = VG_LOTE + 1.

      CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
        TABLES
          T_ARQUIVO          = T_ARQUIVO
          T_REG_CABECALHO    = T_REG_CABECALHO
        CHANGING
          VG_LOTE            = VG_LOTE
        EXCEPTIONS
          NAO_ADMINISTRADORA = 1
          NAO_LOCAL_NEGOCIO  = 2
          OTHERS             = 3.

      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE E002 WITH SY-MSGV1 RAISING NAO_ADMINISTRADORA.
        WHEN 2.
          MESSAGE E003 WITH SY-MSGV1 RAISING NAO_LOCAL_NEGOCIO.
        WHEN 3.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDCASE.

    ENDLOOP.

  ELSE.

    MOVE: DIRETORIO TO WDIR,
          ARQUIVO   TO WARQ.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        DIRECTORY  = WDIR
        FILTER     = WARQ
      TABLES
        FILE_TABLE = IT_FILE_TABLE
        DIR_TABLE  = IT_DIR_TABLE
      EXCEPTIONS
        CNTL_ERROR = 1
        OTHERS     = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
    ENDIF.

    LOOP AT IT_FILE_TABLE INTO WA_FILE_TABLE.

      CONCATENATE DIRETORIO WA_FILE_TABLE-PATHNAME INTO DIR_ARQ.

      CALL FUNCTION 'Z_PFE_ARQUIVO'
        EXPORTING
          ARQUIVO                 = DIR_ARQ
          UNIX                    = SPACE
          LOCAL                   = 'X'
        TABLES
          T_ARQUIVO               = T_ARQUIVO
        EXCEPTIONS
          FILE_OPEN_ERROR         = 1
          FILE_READ_ERROR         = 2
          NO_BATCH                = 3
          GUI_REFUSE_FILETRANSFER = 4
          INVALID_TYPE            = 5
          NO_AUTHORITY            = 6
          UNKNOWN_ERROR           = 7
          BAD_DATA_FORMAT         = 8
          HEADER_NOT_ALLOWED      = 9
          SEPARATOR_NOT_ALLOWED   = 10
          HEADER_TOO_LONG         = 11
          UNKNOWN_DP_ERROR        = 12
          ACCESS_DENIED           = 13
          DP_OUT_OF_MEMORY        = 14
          DISK_FULL               = 15
          DP_TIMEOUT              = 16
          OUTROS                  = 17
          OTHERS                  = 18.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDIF.

      VG_LOTE = VG_LOTE + 1.

      CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
        TABLES
          T_ARQUIVO          = T_ARQUIVO
          T_REG_CABECALHO    = T_REG_CABECALHO
          T_REG_ITENS        = T_REG_ITENS
        CHANGING
          VG_LOTE            = VG_LOTE
        EXCEPTIONS
          NAO_ADMINISTRADORA = 1
          NAO_LOCAL_NEGOCIO  = 2
          OTHERS             = 3.

      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE E002 WITH SY-MSGV1 RAISING NAO_ADMINISTRADORA.
        WHEN 2.
          MESSAGE E003 WITH SY-MSGV1 RAISING NAO_LOCAL_NEGOCIO.
        WHEN 3.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDCASE.

    ENDLOOP.

  ENDIF.

  IF GERAR_LOTES EQ 'X'.

    IF NOT T_REG_ITENS[] IS INITIAL.
      SELECT * INTO TABLE IT_ZLEST0025
        FROM ZLEST0025.
    ENDIF.

    MOVE T_REG_ITENS[] TO T_REG_ITENS_AD[].
    DELETE T_REG_ITENS    WHERE CHVID EQ '1'.
    DELETE T_REG_ITENS_AD WHERE CHVID NE '1'.
    MOVE T_REG_ITENS[] TO T_REG_ITENS_AUX[].

    "Adiantamento
    VG_NM_LOTE_ITEM = 1.
    LOOP AT T_REG_CABECALHO INTO WA_REG_CABECALHO.
      READ TABLE T_ARQUIVO INDEX 1.
      CLEAR: P_TIPCONTABIL.

      READ TABLE T_REG_ITENS_AD WITH KEY NM_LOTE = WA_REG_CABECALHO-NM_LOTE.
      IF NOT SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      WA_REG_CABECALHO-VL_TOTAL_LOTE = 0.
      WA_REG_CABECALHO-VL_CONFI_LOTE = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
        EXPORTING
          P_DT_POSICAO  = WA_REG_CABECALHO-DT_POSICAO
        IMPORTING
          P_TIPCONTABIL = P_TIPCONTABIL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZPFELOTE'
        IMPORTING
          NUMBER                  = ST_LOTE
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OTHERS                  = 3.

      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE E004 WITH 'ZPFELOTE' RAISING INTERVAL_NOT_FOUND.
        WHEN 2.
          MESSAGE E005 WITH '01' 'ZPFELOTE' RAISING NUMBER_RANGE_NOT_INTERN.
        WHEN 3.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDCASE.

      ST_LOTE_AUX = WA_REG_CABECALHO-NM_LOTE.

      WA_REG_CABECALHO-STATUS  = 'I'.
      WA_REG_CABECALHO-NM_LOTE = ST_LOTE.
      LOOP AT T_REG_ITENS_AD INTO WA_REG_ITENS WHERE NM_LOTE EQ ST_LOTE_AUX.
        WL_TABIX = SY-TABIX.
        ADD 1 TO WL_TABIX.
        READ TABLE T_ARQUIVO INDEX WL_TABIX.
        SELECT SINGLE * INTO WA_REG_ITENS_AUX
          FROM ZPFE_LOTE_ITEM
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
           AND CHVID      EQ WA_REG_ITENS-CHVID.

        IF SY-SUBRC IS INITIAL.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                  USING WA_REG_CABECALHO-NR_LOTE_ADM
                                        SPACE
                                        WA_REG_ITENS-CHVID
                                        WA_REG_ITENS-NUCONTRATO
                                        'F'
                                        TEXT-E03
                                        T_ARQUIVO-LINHA
                                        'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        WA_REG_ITENS-NM_LOTE      = ST_LOTE.
        WA_REG_ITENS-STATUS       = 'I'.
        WA_REG_ITENS-NM_LOTE_ITEM = VG_NM_LOTE_ITEM.

        SELECT SINGLE * INTO WA_ZCTE_CIOT
          FROM ZCTE_CIOT
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.

        IF ( SY-SUBRC IS INITIAL ) AND ( WA_REG_ITENS-NUCONTRATO IS NOT INITIAL ).

          IF WA_REG_ITENS-VL_TRANSACAO NE WA_ZCTE_CIOT-VLR_ADIANTAMENTO.
            PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                  USING WA_REG_CABECALHO-NR_LOTE_ADM
                                        SPACE
                                        WA_REG_ITENS-CHVID
                                        WA_REG_ITENS-NUCONTRATO
                                        'F'
                                        TEXT-E04
                                        T_ARQUIVO-LINHA
                                        'E'.
            "Erro Adiantamento
            CONTINUE.
          ENDIF.

          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
            FROM ZCTE_IDENTIFICA
           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.

          WA_REG_ITENS-CD_CIOT    = WA_ZCTE_CIOT-CD_CIOT.
          WA_REG_ITENS-NR_CIOT    = WA_ZCTE_CIOT-NR_CIOT.
          WA_REG_ITENS-DOCNUM	    = WA_ZCTE_CIOT-DOCNUM.
          WA_REG_ITENS-TKNUM      = WA_ZCTE_CIOT-TKNUM.
          WA_REG_ITENS-CTENUM     = WA_ZCTE_IDENTIFICA-NCT.
          WA_REG_ITENS-CTESERIE   = WA_ZCTE_IDENTIFICA-SERIE.
          WA_REG_CABECALHO-VL_TOTAL_LOTE = WA_REG_CABECALHO-VL_TOTAL_LOTE + WA_REG_ITENS-VL_TRANSACAO.

          WA_REG_ITENS-VL_CONFERIDO    = WA_REG_ITENS-VL_TRANSACAO.
          WA_REG_ITENS-VL_PAGO_LOTE    = WA_REG_ITENS-VL_TRANSACAO.
          WA_REG_ITENS-CK_CONFERIDO    = ''.
          WA_REG_ITENS-DS_USUARIO_CONF = SY-UNAME.

          WA_REG_CABECALHO-VL_CONFI_LOTE = WA_REG_CABECALHO-VL_CONFI_LOTE + WA_REG_ITENS-VL_TRANSACAO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_REG_ITENS-NM_LOTE_ITEM
            IMPORTING
              OUTPUT = WA_REG_ITENS-NM_LOTE_ITEM.

          MODIFY ZPFE_LOTE_ITEM FROM WA_REG_ITENS.
          VG_NM_LOTE_ITEM = VG_NM_LOTE_ITEM + 1.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_ITENS-NR_LOTE_ADM
                                       WA_REG_ITENS-NM_LOTE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-S01
                                       SPACE
                                       'S'.
        ELSE.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                  USING WA_REG_CABECALHO-NR_LOTE_ADM
                                        SPACE
                                        WA_REG_ITENS-CHVID
                                        WA_REG_ITENS-NUCONTRATO
                                        'F'
                                        TEXT-E05
                                        T_ARQUIVO-LINHA
                                        'E'.
          "Erro de Contrato não encontrado

        ENDIF.
      ENDLOOP.

      WA_REG_CABECALHO-TPLOTE = 'A'.

      MODIFY ZPFE_LOTE FROM WA_REG_CABECALHO.
      PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                               USING WA_REG_CABECALHO-NR_LOTE_ADM
                                     WA_REG_CABECALHO-NM_LOTE
                                     SPACE
                                     SPACE
                                     'F'
                                     TEXT-S02
                                     SPACE
                                     'S'.
    ENDLOOP.

    "Resto de Chaves
    VG_NM_LOTE_ITEM = 1.
    LOOP AT T_REG_CABECALHO INTO WA_REG_CABECALHO.

      CLEAR: P_TIPCONTABIL.

      READ TABLE T_REG_ITENS WITH KEY NM_LOTE = WA_REG_CABECALHO-NM_LOTE.
      IF NOT SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      WA_REG_CABECALHO-VL_TOTAL_LOTE = 0.
      WA_REG_CABECALHO-VL_CONFI_LOTE = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
        EXPORTING
          P_DT_POSICAO  = WA_REG_CABECALHO-DT_POSICAO
        IMPORTING
          P_TIPCONTABIL = P_TIPCONTABIL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZPFELOTE'
        IMPORTING
          NUMBER                  = ST_LOTE
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OTHERS                  = 3.

      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE E004 WITH 'ZPFELOTE' RAISING INTERVAL_NOT_FOUND.
        WHEN 2.
          MESSAGE E005 WITH '01' 'ZPFELOTE' RAISING NUMBER_RANGE_NOT_INTERN.
        WHEN 3.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS_ERROS.
      ENDCASE.

      ST_LOTE_AUX = WA_REG_CABECALHO-NM_LOTE.

      WA_REG_CABECALHO-STATUS  = 'I'.
      WA_REG_CABECALHO-NM_LOTE = ST_LOTE.

      "Verificações de Saldo de Frete
      LOOP AT T_REG_ITENS INTO WA_REG_ITENS WHERE NM_LOTE EQ ST_LOTE_AUX AND CHVID EQ '2'.
        WL_TABIX = SY-TABIX.
        ADD 1 TO WL_TABIX.
        READ TABLE T_ARQUIVO INDEX WL_TABIX.
        READ TABLE IT_ZLEST0025 WITH KEY CHVID = WA_REG_ITENS-CHVID.

        IF ( WA_REG_ITENS-CHVID IS INITIAL ) OR ( NOT SY-SUBRC IS INITIAL ).
          CONTINUE.
        ENDIF.

        SELECT SINGLE * INTO WA_REG_ITENS_AUX
          FROM ZPFE_LOTE_ITEM
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
           AND CHVID      EQ WA_REG_ITENS-CHVID.

        IF SY-SUBRC IS INITIAL.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                  USING WA_REG_CABECALHO-NR_LOTE_ADM
                                        SPACE
                                        WA_REG_ITENS-CHVID
                                        WA_REG_ITENS-NUCONTRATO
                                        'F'
                                        TEXT-E03
                                        T_ARQUIVO-LINHA
                                        'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        WA_REG_ITENS-NM_LOTE      = ST_LOTE.
        WA_REG_ITENS-STATUS       = 'I'.
        WA_REG_ITENS-NM_LOTE_ITEM = VG_NM_LOTE_ITEM.

        SELECT SINGLE * INTO WA_ZCTE_CIOT
          FROM ZCTE_CIOT
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.

        IF ( SY-SUBRC IS INITIAL ) AND ( WA_REG_ITENS-NUCONTRATO IS NOT INITIAL ).

          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
            FROM ZCTE_IDENTIFICA
           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.

          IT_VALORES-CD_CIOT   = WA_ZCTE_CIOT-CD_CIOT.
          IT_VALORES-DOCNUM    = WA_ZCTE_CIOT-DOCNUM.
          IT_VALORES-VL_QUEBRA = 0.
          IT_VALORES-VL_PERDA  = 0.

          WA_REG_ITENS-CD_CIOT     = WA_ZCTE_CIOT-CD_CIOT.
          WA_REG_ITENS-NR_CIOT     = WA_ZCTE_CIOT-NR_CIOT.
          WA_REG_ITENS-DOCNUM	     = WA_ZCTE_CIOT-DOCNUM.
          WA_REG_ITENS-TKNUM       = WA_ZCTE_CIOT-TKNUM.
          WA_REG_ITENS-PESO_ORIGEM = WA_ZCTE_CIOT-QUANTIDADE.
          WA_REG_ITENS-CTENUM      = WA_ZCTE_IDENTIFICA-NCT.
          WA_REG_ITENS-CTESERIE    = WA_ZCTE_IDENTIFICA-SERIE.
          WA_REG_CABECALHO-VL_TOTAL_LOTE = WA_REG_CABECALHO-VL_TOTAL_LOTE + WA_REG_ITENS-VL_TRANSACAO.

          IF P_TIPCONTABIL EQ 'FC'.
            WA_REG_ITENS-PESO_CHEGADA = WA_REG_ITENS-PESO_IMPORTADO.

            "if wa_reg_itens-chvid eq '2'.
*            WA_ZCTE_CIOT-VLR_FRETE = ( ( WA_ZCTE_CIOT-VLR_FRETE -
*                                     WA_ZCTE_CIOT-VLR_ADIANTAMENTO -
**                                     wa_zcte_ciot-vlr_seguro -
*                                     WA_ZCTE_CIOT-VLR_IMPOSTOS ) + WA_ZCTE_CIOT-VLR_IOF ).

            WA_ZCTE_CIOT-VLR_FRETE = WA_ZCTE_CIOT-VLR_FRETE -
                                     WA_ZCTE_CIOT-VLR_ADIANTAMENTO -
                                     WA_ZCTE_CIOT-VLR_SEGURO -
                                     WA_ZCTE_CIOT-VLR_IMPOSTOS.

            IF WA_REG_ITENS-PESO_CHEGADA GT WA_REG_ITENS-PESO_ORIGEM.
              WA_REG_ITENS-PESO_CHEGADA = WA_REG_ITENS-PESO_ORIGEM.
            ENDIF.
            WA_REG_ITENS-VL_PAGO_LOTE = WA_ZCTE_CIOT-VLR_FRETE.

            IF ( WA_REG_ITENS-PESO_CHEGADA LT WA_REG_ITENS-PESO_ORIGEM ) OR
               ( WA_REG_ITENS-VL_TRANSACAO GT WA_ZCTE_CIOT-VLR_FRETE ).
              VG_DIFERENCA = WA_REG_ITENS-PESO_ORIGEM - WA_REG_ITENS-PESO_CHEGADA.

              "Valor de Quebra
              IT_VALORES-VL_QUEBRA  = ( VG_DIFERENCA * ( WA_ZCTE_CIOT-VLR_UNIT_FRETE / 1000 ) ).
              "Valor da Perda
              VG_TOLERAVEL          = WA_REG_ITENS-PESO_ORIGEM * ( WA_ZCTE_CIOT-PERC_TOLERANCIA / 100 ).
              IF VG_DIFERENCA GT VG_TOLERAVEL.
                IT_VALORES-VL_PERDA = ( ( VG_DIFERENCA - VG_TOLERAVEL ) * WA_ZCTE_CIOT-VLR_UNIT_MERC  ).
              ENDIF.
            ENDIF.
            "else.
            "  wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_transacao.
            "endif.
          ELSEIF  P_TIPCONTABIL EQ 'FS'.
            WA_REG_ITENS-VL_CONFERIDO    = WA_REG_ITENS-VL_TRANSACAO.
            WA_REG_ITENS-VL_PAGO_LOTE    = WA_REG_ITENS-VL_TRANSACAO.
            WA_REG_ITENS-PESO_CHEGADA    = WA_REG_ITENS-PESO_IMPORTADO.
            WA_REG_ITENS-CK_CONFERIDO    = 'X'.
            WA_REG_ITENS-DS_USUARIO_CONF = SY-UNAME.
          ENDIF.
          APPEND IT_VALORES.
          IF IT_ZLEST0025-NATUREZACHVID EQ 'S'.
            WA_REG_ITENS-VL_PAGO_LOTE = WA_REG_ITENS-VL_PAGO_LOTE * ( -1 ).
          ENDIF.

          WA_REG_CABECALHO-VL_CONFI_LOTE = WA_REG_CABECALHO-VL_CONFI_LOTE + WA_REG_ITENS-VL_PAGO_LOTE.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_REG_ITENS-NM_LOTE_ITEM
            IMPORTING
              OUTPUT = WA_REG_ITENS-NM_LOTE_ITEM.

          MODIFY ZPFE_LOTE_ITEM FROM WA_REG_ITENS.
          VG_NM_LOTE_ITEM = VG_NM_LOTE_ITEM + 1.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_ITENS-NR_LOTE_ADM
                                       WA_REG_ITENS-NM_LOTE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-S01
                                       SPACE
                                       'S'.
        ELSE.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_CABECALHO-NR_LOTE_ADM
                                       SPACE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-E05
                                       T_ARQUIVO-LINHA
                                       'E'.
          "Erro de Contrato não encontrado
        ENDIF.
      ENDLOOP.

      LOOP AT T_REG_ITENS INTO WA_REG_ITENS WHERE NM_LOTE EQ ST_LOTE_AUX AND CHVID NE '2'.
        WL_TABIX = SY-TABIX.
        ADD 1 TO WL_TABIX.
        READ TABLE T_ARQUIVO INDEX WL_TABIX.
        READ TABLE IT_ZLEST0025 WITH KEY CHVID = WA_REG_ITENS-CHVID.

        IF ( WA_REG_ITENS-CHVID IS INITIAL ) OR ( NOT SY-SUBRC IS INITIAL ).
          CONTINUE.
        ENDIF.

        SELECT SINGLE * INTO WA_REG_ITENS_AUX
          FROM ZPFE_LOTE_ITEM
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO
           AND CHVID      EQ WA_REG_ITENS-CHVID.

        IF SY-SUBRC IS INITIAL.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_CABECALHO-NR_LOTE_ADM
                                       SPACE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-E03
                                       T_ARQUIVO-LINHA
                                       'E'.
          "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        WA_REG_ITENS-NM_LOTE      = ST_LOTE.
        WA_REG_ITENS-STATUS       = 'I'.
        WA_REG_ITENS-NM_LOTE_ITEM = VG_NM_LOTE_ITEM.

        SELECT SINGLE * INTO WA_ZCTE_CIOT
          FROM ZCTE_CIOT
         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.

        IF ( SY-SUBRC IS INITIAL ) AND ( WA_REG_ITENS-NUCONTRATO IS NOT INITIAL ).

          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
            FROM ZCTE_IDENTIFICA
           WHERE DOCNUM EQ WA_ZCTE_CIOT-DOCNUM.

          WA_REG_ITENS-CD_CIOT     = WA_ZCTE_CIOT-CD_CIOT.
          WA_REG_ITENS-NR_CIOT     = WA_ZCTE_CIOT-NR_CIOT.
          WA_REG_ITENS-DOCNUM	     = WA_ZCTE_CIOT-DOCNUM.
          WA_REG_ITENS-TKNUM       = WA_ZCTE_CIOT-TKNUM.
          WA_REG_ITENS-PESO_ORIGEM = WA_ZCTE_CIOT-QUANTIDADE.
          WA_REG_ITENS-CTENUM      = WA_ZCTE_IDENTIFICA-NCT.
          WA_REG_ITENS-CTESERIE    = WA_ZCTE_IDENTIFICA-SERIE.
          IF IT_ZLEST0025-NATUREZACHVID EQ 'S'.
            WA_REG_ITENS-VL_TRANSACAO = WA_REG_ITENS-VL_TRANSACAO * ( -1 ).
          ENDIF.
          WA_REG_CABECALHO-VL_TOTAL_LOTE = WA_REG_CABECALHO-VL_TOTAL_LOTE + WA_REG_ITENS-VL_TRANSACAO.

          IF P_TIPCONTABIL EQ 'FC'.
            WA_REG_ITENS-VL_DIFERENCA  = 0.

            CASE WA_REG_ITENS-CHVID.
              WHEN '30'.
                READ TABLE IT_VALORES WITH KEY CD_CIOT = WA_ZCTE_CIOT-CD_CIOT
                                               DOCNUM  = WA_ZCTE_CIOT-DOCNUM.
                IF SY-SUBRC IS INITIAL.
                  VG_TABIX = SY-TABIX.
                  WA_REG_ITENS-VL_PAGO_LOTE = IT_VALORES-VL_QUEBRA * -1.
                  IT_VALORES-VL_QUEBRA      = 0.
                  MODIFY IT_VALORES INDEX VG_TABIX TRANSPORTING VL_QUEBRA.
                ENDIF.
                "Quebra
              WHEN '31'.
                "Perda
                READ TABLE IT_VALORES WITH KEY CD_CIOT = WA_ZCTE_CIOT-CD_CIOT
                                               DOCNUM  = WA_ZCTE_CIOT-DOCNUM.
                if sy-SUBRC is INITIAL.
                VG_TABIX = SY-TABIX.
                WA_REG_ITENS-VL_PAGO_LOTE = IT_VALORES-VL_PERDA * -1.
                IT_VALORES-VL_PERDA       = 0.
                MODIFY IT_VALORES INDEX VG_TABIX TRANSPORTING VL_PERDA.
                endif.
              WHEN OTHERS.
                WA_REG_ITENS-VL_PAGO_LOTE = WA_REG_ITENS-VL_TRANSACAO.
            ENDCASE.

          ELSEIF  P_TIPCONTABIL EQ 'FS'.
            WA_REG_ITENS-VL_CONFERIDO    = WA_REG_ITENS-VL_TRANSACAO.
            WA_REG_ITENS-VL_PAGO_LOTE    = WA_REG_ITENS-VL_TRANSACAO.
            WA_REG_ITENS-CK_CONFERIDO    = 'X'.
            WA_REG_ITENS-DS_USUARIO_CONF = SY-UNAME.
          ENDIF.

          WA_REG_CABECALHO-VL_CONFI_LOTE = WA_REG_CABECALHO-VL_CONFI_LOTE + WA_REG_ITENS-VL_PAGO_LOTE.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_REG_ITENS-NM_LOTE_ITEM
            IMPORTING
              OUTPUT = WA_REG_ITENS-NM_LOTE_ITEM.

          MODIFY ZPFE_LOTE_ITEM FROM WA_REG_ITENS.
          VG_NM_LOTE_ITEM = VG_NM_LOTE_ITEM + 1.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_ITENS-NR_LOTE_ADM
                                       WA_REG_ITENS-NM_LOTE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-S01
                                       SPACE
                                       'S'.
        ELSE.
          PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_CABECALHO-NR_LOTE_ADM
                                       SPACE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-E05
                                       T_ARQUIVO-LINHA
                                       'E'.
          "Erro de Contrato não encontrado
        ENDIF.
      ENDLOOP.

      IF P_TIPCONTABIL EQ 'FC'.
        LOOP AT IT_VALORES WHERE ( VL_QUEBRA GT 0 OR VL_PERDA GT 0 ).

          WA_REG_ITENS-NM_LOTE = ST_LOTE.
          WA_REG_ITENS-STATUS  = 'I'.

          SELECT SINGLE * INTO WA_ZCTE_CIOT
            FROM ZCTE_CIOT
           WHERE CD_CIOT EQ IT_VALORES-CD_CIOT.

          SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
            FROM ZCTE_IDENTIFICA
           WHERE DOCNUM EQ IT_VALORES-DOCNUM.

          WA_REG_ITENS-CD_CIOT       = WA_ZCTE_CIOT-CD_CIOT.
          WA_REG_ITENS-NR_CIOT       = WA_ZCTE_CIOT-NR_CIOT.
          WA_REG_ITENS-DOCNUM	       = WA_ZCTE_CIOT-DOCNUM.
          WA_REG_ITENS-TKNUM         = WA_ZCTE_CIOT-TKNUM.
          WA_REG_ITENS-CTENUM        = WA_ZCTE_IDENTIFICA-NCT.
          WA_REG_ITENS-CTESERIE      = WA_ZCTE_IDENTIFICA-SERIE.

          WA_REG_ITENS-VL_PAGO_LOTE  = 0.
          WA_REG_ITENS-VL_TRANSACAO  = 0.
          WA_REG_ITENS-VL_DIFERENCA  = 0.

          IF IT_VALORES-VL_QUEBRA GT 0.
            READ TABLE IT_ZLEST0025 WITH KEY CHVID = '30'.
            WA_REG_ITENS-VL_PAGO_LOTE = IT_VALORES-VL_QUEBRA * ( -1 ).
            WA_REG_CABECALHO-VL_CONFI_LOTE = WA_REG_CABECALHO-VL_CONFI_LOTE + WA_REG_ITENS-VL_PAGO_LOTE.
            WA_REG_ITENS-CHVID = '30'.
            WA_REG_ITENS-NM_LOTE_ITEM = VG_NM_LOTE_ITEM.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_REG_ITENS-NM_LOTE_ITEM
              IMPORTING
                OUTPUT = WA_REG_ITENS-NM_LOTE_ITEM.

            MODIFY ZPFE_LOTE_ITEM FROM WA_REG_ITENS.
            VG_NM_LOTE_ITEM = VG_NM_LOTE_ITEM + 1.
          ENDIF.
          IF IT_VALORES-VL_PERDA GT 0.
            READ TABLE IT_ZLEST0025 WITH KEY CHVID = '31'.
            WA_REG_ITENS-VL_PAGO_LOTE = IT_VALORES-VL_PERDA * ( -1 ).
            WA_REG_CABECALHO-VL_CONFI_LOTE = WA_REG_CABECALHO-VL_CONFI_LOTE + WA_REG_ITENS-VL_PAGO_LOTE.
            WA_REG_ITENS-CHVID = '31'.
            WA_REG_ITENS-NM_LOTE_ITEM = VG_NM_LOTE_ITEM.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_REG_ITENS-NM_LOTE_ITEM
              IMPORTING
                OUTPUT = WA_REG_ITENS-NM_LOTE_ITEM.

            MODIFY ZPFE_LOTE_ITEM FROM WA_REG_ITENS.
            VG_NM_LOTE_ITEM = VG_NM_LOTE_ITEM + 1.
            PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                 USING WA_REG_ITENS-NR_LOTE_ADM
                                       WA_REG_ITENS-NM_LOTE
                                       WA_REG_ITENS-CHVID
                                       WA_REG_ITENS-NUCONTRATO
                                       'F'
                                       TEXT-S01
                                       SPACE
                                       'S'.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CLEAR: IT_VALORES[].

      WA_REG_CABECALHO-TPLOTE = 'S'.
      MODIFY ZPFE_LOTE FROM WA_REG_CABECALHO.
      PERFORM INSERT_LINE_LOG IN PROGRAM SAPMZLES003
                                      USING WA_REG_CABECALHO-NR_LOTE_ADM
                                            WA_REG_CABECALHO-NM_LOTE
                                            SPACE
                                            SPACE
                                            'F'
                                            TEXT-S02
                                            SPACE
                                            'S'.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
