*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 20/01/2014                                              &*
*& Descrição: Relatório de Controle de Lubrificações                  &*
*& Transação: ZPM0013                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZPMR0001.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_V_EQUI,
         EQUNR TYPE V_EQUI-EQUNR,
         EQART TYPE V_EQUI-EQART,
         IWERK TYPE V_EQUI-IWERK,
         OBJNR TYPE V_EQUI-OBJNR,
         EQKTU TYPE V_EQUI-EQKTU,
         SWERK TYPE V_EQUI-SWERK,
       END OF TY_V_EQUI,

       BEGIN OF TY_AFIH,
         EQUNR TYPE AFIH-EQUNR,
         AUFNR TYPE AFIH-AUFNR,
       END OF TY_AFIH,

       BEGIN OF TY_T370FLD_STN,
         STATION   TYPE T370FLD_STN-STATION,
         STORAGE   TYPE T370FLD_STN-STORAGE,
         PLANT     TYPE T370FLD_STN-PLANT,
       END OF TY_T370FLD_STN,

       BEGIN OF TY_T370FLD_STN_T,
         STATION   TYPE T370FLD_STN_T-STATION,
         TYPE_TEXT TYPE T370FLD_STN_T-TYPE_TEXT,
       END OF TY_T370FLD_STN_T,

       BEGIN OF TY_T370_AUX,
         STATION   TYPE T370FLD_STN-STATION,
         TYPE_TEXT TYPE T370FLD_STN_T-TYPE_TEXT,
         STORAGE   TYPE T370FLD_STN-STORAGE,
         PLANT     TYPE T370FLD_STN-PLANT,
       END OF TY_T370_AUX,

       BEGIN OF TY_IMRG,
         POINT TYPE IMRG-POINT,
         IDATE TYPE IMRG-IDATE,
         CANCL TYPE IMRG-CANCL,
         MDOCM TYPE IMRG-MDOCM,
         READG TYPE IMRG-READG,
         RECDU TYPE IMRG-RECDU,
         VLCOD TYPE IMRG-VLCOD,
         ITIME TYPE IMRG-ITIME,
       END OF TY_IMRG,

       BEGIN OF TY_MSEG,
         MATNR      TYPE MSEG-MATNR,
         WERKS      TYPE MSEG-WERKS,
         LGORT      TYPE MSEG-LGORT,
         AUFNR      TYPE MSEG-AUFNR,
         MJAHR      TYPE MSEG-MJAHR,
         BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
         MBLNR      TYPE MSEG-MBLNR,
         DMBTR      TYPE MSEG-DMBTR,
         MENGE      TYPE MSEG-MENGE,
         MEINS      TYPE MSEG-MEINS,
         SMBLN      TYPE MSEG-SMBLN,
       END OF TY_MSEG,

       BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_T370K_T,
         EQART TYPE T370K_T-EQART,
         EARTX TYPE T370K_T-EARTX,
       END OF TY_T370K_T,

       BEGIN OF TY_QPCT,
         CODEGRUPPE TYPE QPCT-CODEGRUPPE,
         CODE       TYPE QPCT-CODE,
         KURZTEXT   TYPE QPCT-KURZTEXT,
       END OF TY_QPCT,

       BEGIN OF TY_SAIDA,
         TYPE_TEXT   TYPE V_T370FLD_STN-TYPE_TEXT,
         EARTX       TYPE T370K_T-EARTX,
         OPERACAO(50),    "TYPE QPCT-KURZTEXT,
         IDATE       TYPE IMRG-IDATE,
         EQUIPAMENTO(50),
         READG       TYPE IMRC_TOTAC ,
         READG_C     TYPE IMRC_TOTAC,
         RECDU       TYPE IMRG-RECDU,
         MATERIAL(50),
         MENGE       TYPE MSEG-MENGE,
         MEINS       TYPE MSEG-MEINS,
         AUFNR       TYPE MSEG-AUFNR,
       END OF TY_SAIDA.


DATA: RG_MATNR TYPE RANGE OF MSEG-MATNR,
      WG_MATNR LIKE LINE OF RG_MATNR,
      RG_DOCDAT TYPE RANGE OF IMRG-IDATE,
      WG_DOCDAT LIKE LINE OF RG_DOCDAT.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_V_EQUI   TYPE TABLE OF TY_V_EQUI,
      T_AFIH     TYPE TABLE OF TY_AFIH,
      T_T370     TYPE TABLE OF TY_T370FLD_STN,
      T_T370_T   TYPE TABLE OF TY_T370FLD_STN_T,
      T_T370_AUX TYPE TABLE OF TY_T370_AUX,
*        T_V_T037  TYPE TABLE OF TY_V_T370FLD_STN,
      T_IMRG     TYPE TABLE OF TY_IMRG,
      T_MSEG     TYPE TABLE OF TY_MSEG,
      T_MSEG_AUX TYPE TABLE OF TY_MSEG,
      T_MAKT     TYPE TABLE OF TY_MAKT,
      T_T370K_T  TYPE TABLE OF TY_T370K_T,
      T_QPCT     TYPE TABLE OF TY_QPCT,
      T_SAIDA    TYPE TABLE OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_V_EQUI   TYPE TY_V_EQUI,
      WA_AFIH     TYPE TY_AFIH,
      WA_T370     TYPE TY_T370FLD_STN,
      WA_T370_T   TYPE TY_T370FLD_STN_T,
      WA_T370_AUX TYPE TY_T370_AUX,
*        WA_V_T037  TYPE TY_V_T370FLD_STN,
      WA_IMRG     TYPE TY_IMRG,
      WA_MSEG     TYPE TY_MSEG,
      WA_MSEG_AUX TYPE TY_MSEG,
      WA_MAKT     TYPE TY_MAKT,
      WA_T370K_T  TYPE TY_T370K_T,
      WA_QPCT     TYPE TY_QPCT,
      WA_SAIDA    TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      LT_SORT      TYPE SLIS_T_SORTINFO_ALV,
      LS_SORT      TYPE SLIS_SORTINFO_ALV,
      INIT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_EQUNR FOR WA_V_EQUI-EQUNR,
                S_DOCDAT FOR WA_IMRG-IDATE NO-EXTENSION OBLIGATORY,
                S_STATIO FOR WA_T370-STATION,
                S_EQART  FOR WA_V_EQUI-EQART,
                S_IWERK  FOR WA_V_EQUI-IWERK NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK A1.

START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  DATA: TL_SETLEAF TYPE TABLE OF SETLEAF WITH HEADER LINE,
        WL_INDEX   TYPE SY-INDEX.

  SELECT EQUNR EQART IWERK OBJNR EQKTU SWERK
    FROM V_EQUI
      INTO TABLE T_V_EQUI
        WHERE EQUNR IN S_EQUNR
          AND EQART IN S_EQART
          AND IWERK IN S_IWERK.

  IF SY-SUBRC IS INITIAL.
    SELECT EQUNR AUFNR
      FROM AFIH
        INTO TABLE T_AFIH
        FOR ALL ENTRIES IN T_V_EQUI
          WHERE EQUNR EQ T_V_EQUI-EQUNR.

    IF SY-SUBRC IS INITIAL.
*      WL_MJAHR-LOW  = S_DOCDAT-LOW(4).
*      WL_MJAHR-HIGH = S_DOCDAT-HIGH(4).

      IF S_DOCDAT-HIGH IS INITIAL.
        WG_DOCDAT-SIGN   = 'I'.
        WG_DOCDAT-OPTION = 'EQ'.
        WG_DOCDAT-LOW    = S_DOCDAT-LOW(4).
        APPEND WG_DOCDAT TO RG_DOCDAT.
        CLEAR WG_DOCDAT.

      ELSE.
        WG_DOCDAT-SIGN   = 'I'.
        WG_DOCDAT-OPTION = 'EQ'.
        WG_DOCDAT-LOW    = S_DOCDAT-LOW(4).
        WG_DOCDAT-HIGH   = S_DOCDAT-HIGH(4).
        APPEND WG_DOCDAT TO RG_DOCDAT.
        CLEAR WG_DOCDAT.
      ENDIF.

      SELECT *
        FROM SETLEAF
          INTO TABLE TL_SETLEAF
            WHERE SETNAME EQ 'MAGGI_PM_LUBRIFICANTES'.

      LOOP AT  TL_SETLEAF.
        MOVE: TL_SETLEAF-VALSIGN   TO WG_MATNR-SIGN,
              TL_SETLEAF-VALOPTION TO WG_MATNR-OPTION,
              TL_SETLEAF-VALFROM   TO WG_MATNR-LOW.
        APPEND WG_MATNR TO RG_MATNR.
        CLEAR WG_MATNR.
      ENDLOOP.


      SELECT MATNR WERKS LGORT AUFNR MJAHR BUDAT_MKPF MBLNR DMBTR MENGE MEINS  SMBLN
        FROM MSEG
          INTO TABLE T_MSEG
          FOR ALL ENTRIES IN T_AFIH
            WHERE WERKS      IN S_IWERK
              AND MATNR      IN RG_MATNR      "buscar materiais no SET MAGGI_PM_LUBRIFICANTES - RGSBL-FROM
              AND AUFNR      EQ T_AFIH-AUFNR
              AND MJAHR      IN RG_DOCDAT
              AND BUDAT_MKPF IN S_DOCDAT.

*      T_MSEG_AUX[] = T_MSEG[].

      LOOP AT T_MSEG INTO WA_MSEG.
*        WL_INDEX = SY-TABIX.

        READ TABLE T_MSEG TRANSPORTING NO FIELDS
          WITH KEY SMBLN = WA_MSEG-MBLNR.
*          with key MBLNR = WA_MSEG-SMBLN .
*
        IF SY-SUBRC IS INITIAL.
          DELETE T_MSEG.
        ENDIF.

      ENDLOOP.

      DELETE T_MSEG[] WHERE SMBLN NE SPACE.

      IF T_MSEG[] IS NOT INITIAL.
        SELECT STATION STORAGE PLANT
          FROM T370FLD_STN
            INTO TABLE T_T370
            FOR ALL ENTRIES IN T_MSEG
              WHERE STORAGE EQ T_MSEG-LGORT
                AND PLANT   EQ T_MSEG-WERKS
                AND STATION IN S_STATIO.

        IF SY-SUBRC IS INITIAL.
          SELECT STATION TYPE_TEXT
            FROM T370FLD_STN_T
              INTO TABLE T_T370_T
              FOR ALL ENTRIES IN T_T370
                WHERE STATION EQ T_T370-STATION.

          LOOP AT T_T370 INTO WA_T370.
            READ TABLE T_T370_T INTO WA_T370_T
              WITH KEY STATION = WA_T370-STATION.

            MOVE: WA_T370-STATION     TO WA_T370_AUX-STATION,
                  WA_T370_T-TYPE_TEXT TO WA_T370_AUX-TYPE_TEXT,
                  WA_T370-STORAGE     TO WA_T370_AUX-STORAGE,
                  WA_T370-PLANT       TO WA_T370_AUX-PLANT.
            APPEND: WA_T370_AUX TO T_T370_AUX.
            CLEAR: WA_T370_AUX.
          ENDLOOP.
        ENDIF.

        SELECT MATNR MAKTX
          FROM MAKT
            INTO TABLE T_MAKT
            FOR ALL ENTRIES IN T_MSEG
              WHERE MATNR EQ T_MSEG-MATNR.
      ENDIF.
    ENDIF.

    SELECT EQART EARTX
      FROM T370K_T
        INTO TABLE T_T370K_T
        FOR ALL ENTRIES IN T_V_EQUI
          WHERE EQART EQ T_V_EQUI-EQART
            AND SPRAS EQ SY-LANGU.

  ENDIF.

  SELECT POINT IDATE CANCL MDOCM READG RECDU VLCOD ITIME
    FROM IMRG
      INTO TABLE T_IMRG
        WHERE IDATE IN S_DOCDAT
          AND CANCL = SPACE.

  IF SY-SUBRC IS INITIAL.
    SELECT CODEGRUPPE CODE KURZTEXT
      FROM QPCT
        INTO TABLE T_QPCT
        FOR ALL ENTRIES IN T_IMRG
          WHERE CODE EQ T_IMRG-VLCOD
            AND CODEGRUPPE EQ 'H-ATIV'.

  ENDIF.



*  LOOP AT T_MSEG INTO WA_MSEG.
*    READ TABLE T_T370_AUX INTO WA_T370_AUX
*      WITH KEY STORAGE = WA_MSEG-LGORT.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      DELETE: T_MSEG FROM WA_MSEG.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .
  DATA: TL_DIIMPT   TYPE TABLE OF DIIMPT,
        WL_DIIMPT   TYPE DIIMPT,
        WL_DEC(16)  TYPE P DECIMALS 1.

  CLEAR: WA_SAIDA, WA_V_EQUI, WA_IMRG, WA_T370_AUX, WA_MSEG, WA_MAKT, WA_T370K_T, WA_QPCT.

  SORT: T_IMRG     BY POINT IDATE DESCENDING MDOCM DESCENDING ITIME DESCENDING,
        T_T370K_T  BY EQART,
        T_AFIH     BY EQUNR AUFNR,
        T_QPCT     BY CODE CODEGRUPPE,
        T_MSEG     BY MATNR AUFNR BUDAT_MKPF,
        T_MAKT     BY MATNR,
        T_T370_AUX BY STORAGE PLANT.

  LOOP AT T_V_EQUI INTO WA_V_EQUI.
    REFRESH: TL_DIIMPT.

    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
        EXPORTING
          I_EQUNR          = WA_V_EQUI-EQUNR
        TABLES
*            ET_RETURN1       =
          ET_DIIMPT        = TL_DIIMPT.

    IF TL_DIIMPT[] IS NOT INITIAL.
      LOOP AT TL_DIIMPT INTO WL_DIIMPT
*        WHERE   ( PSORT EQ 'FILTROS'
*                 AND ATNAM EQ 'F-FILTRO' )
*             OR (    PSORT EQ 'OLEO DO MOTOR'
*                 AND ATNAM EQ 'H-OLEO'      ).
        WHERE   ( PSORT EQ 'ODOMETRO'
                 AND ATNAM EQ 'ODOMETRO' )
             OR (    PSORT EQ 'HORIMETRO'
                 AND ATNAM EQ 'HORIMETRO' ).

        READ TABLE T_T370K_T INTO WA_T370K_T
          WITH KEY EQART = WA_V_EQUI-EQART
                   BINARY SEARCH.

        LOOP AT T_AFIH INTO WA_AFIH
          WHERE EQUNR EQ WA_V_EQUI-EQUNR.
*                         BINARY SEARCH.

*          IF SY-SUBRC IS INITIAL.
          LOOP AT T_MSEG INTO WA_MSEG
            WHERE AUFNR EQ WA_AFIH-AUFNR.
*                       BINARY SEARCH.

*            IF SY-SUBRC IS INITIAL.
            READ TABLE T_MAKT INTO WA_MAKT
              WITH KEY MATNR = WA_MSEG-MATNR
                       BINARY SEARCH.

            READ TABLE T_T370_AUX INTO WA_T370_AUX
              WITH KEY STORAGE = WA_MSEG-LGORT
                       PLANT   = WA_MSEG-WERKS
                       BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.


*            LOOP AT T_IMRG INTO WA_IMRG
*              WHERE POINT = WL_DIIMPT-POINT
*                and IDATE = WA_MSEG-BUDAT_MKPF.
              READ TABLE T_IMRG INTO WA_IMRG
                 WITH KEY POINT = WL_DIIMPT-POINT
                          IDATE = WA_MSEG-BUDAT_MKPF.

              IF SY-SUBRC IS INITIAL.
                READ TABLE T_QPCT INTO WA_QPCT
                  WITH KEY CODE       = WA_IMRG-VLCOD
                           CODEGRUPPE = 'H-ATIV'
                           BINARY SEARCH.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    INPUT  = WA_MSEG-MATNR
                  IMPORTING
                    OUTPUT = WA_MSEG-MATNR.


                CONCATENATE WA_MSEG-MATNR WA_MAKT-MAKTX INTO WA_SAIDA-MATERIAL
                            SEPARATED BY '-'.

                WA_SAIDA-TYPE_TEXT = WA_T370_AUX-TYPE_TEXT.
*          ENDIF.

                WA_SAIDA-MENGE     = WA_MSEG-MENGE.
                WA_SAIDA-MEINS     = WA_MSEG-MEINS.
                WA_SAIDA-AUFNR     = WA_MSEG-AUFNR.

*        ENDIF.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    INPUT  = WA_V_EQUI-EQUNR
                  IMPORTING
                    OUTPUT = WA_V_EQUI-EQUNR.


                WA_SAIDA-EARTX = WA_T370K_T-EARTX.
                CONCATENATE WA_V_EQUI-EQUNR WA_V_EQUI-EQKTU INTO WA_SAIDA-EQUIPAMENTO
                            SEPARATED BY SPACE.

                CONCATENATE WA_IMRG-VLCOD WA_QPCT-KURZTEXT INTO WA_SAIDA-OPERACAO
                            SEPARATED BY SPACE.
                WA_SAIDA-EARTX = WA_T370K_T-EARTX.
                WA_SAIDA-IDATE = WA_IMRG-IDATE.
                WA_SAIDA-RECDU = WA_IMRG-RECDU.

                CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                  EXPORTING
                    CHAR_UNIT       = WL_DIIMPT-MSEHI
                    DECIMALS        = 0
                    EXPONENT        = 0
                    FLTP_VALUE_SI   = WA_IMRG-READG
                    INDICATOR_VALUE = 'X'
                    MASC_SYMBOL     = ' '
                  IMPORTING
                    CHAR_VALUE      = WA_SAIDA-READG_C.

*              WA_SAIDA-READG = WA_IMRG-READG.

*            ENDIF.

                APPEND WA_SAIDA TO T_SAIDA.
                CLEAR: WA_SAIDA,  WA_IMRG, WA_QPCT.
              ENDIF.
              CLEAR:  WA_MAKT, WA_T370_AUX.
            ENDIF.
          ENDLOOP.
          CLEAR: WA_MSEG.
        ENDLOOP.
        CLEAR: WA_T370K_T, WA_AFIH.
      ENDLOOP.
    ENDIF.
    CLEAR: WL_DIIMPT.
  ENDLOOP.
  SORT T_SAIDA BY IDATE.
ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT          TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.
*  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
*  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = V_REPORT
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
     IT_FIELDCAT                       = ESTRUTURA[]
     IS_LAYOUT                         = WL_LAYOUT
     I_SAVE                            = 'A'
     IT_EVENTS                         = EVENTS
     IS_PRINT                          = T_PRINT
    TABLES
      T_OUTTAB                          = T_SAIDA.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.


  PERFORM F_CARREGAR_EVENTOS USING:
*                                   SLIS_EV_USER_COMMAND  'XUSER_COMMAND',
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  PERFORM MONTAR_ESTRUTURA USING:
        1  'V_T370FLD_STN'    'TYPE_TEXT'     'T_SAIDA' 'TYPE_TEXT'        'Ponto de Abantecimento'       ' ' ' ',
        2  'T370K_T'          'EARTX'         'T_SAIDA' 'EARTX'            'Tipo de veículo'              ' ' ' ',
*        3  ' '                ' '             'T_SAIDA' 'OPERACAO'         'Operação'                     ' ' ' ',
        4  'IMRG'             'IDATE'         'T_SAIDA' 'IDATE'            'Data'                         ' ' ' ',
        5  ' '                ' '             'T_SAIDA' 'EQUIPAMENTO'      'Equipamento'                  ' ' ' ',
        6  ' '                ' '             'T_SAIDA' 'READG_C'          'Pto Med. Atual'               ' ' ' ',
        7  'IMRG'             'RECDU'         'T_SAIDA' 'RECDU'            'Uni Med. Pto Medição'         ' ' ' ',
        8  ' '                ' '             'T_SAIDA' 'MATERIAL'         'Material'                     ' ' ' ',
        9  'MSEG'             'MENGE'         'T_SAIDA' 'MENGE'            'Quantidade'                   ' ' ' ',
       10  'MSEG'             'MEINS'         'T_SAIDA' 'MEINS'            'UM'                           ' ' ' ',
       11  'MSEG'             'AUFNR'         'T_SAIDA' 'AUFNR'            'Ordem de serviço'             ' ' ' '.



ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-EDIT          = P_EDIT.
  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = 'LOGO_MAGGI'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.
  V_REPORT = SY-REPID.

  DATA: WL_DOCDAT_LOW(10),
        WL_DOCDAT_HIGH(10),
        WL_TEXT(50).

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

  IF S_DOCDAT-HIGH IS NOT INITIAL.
    CONCATENATE S_DOCDAT-LOW+6(2)  '/' S_DOCDAT-LOW+4(2)  '/' S_DOCDAT-LOW(4)  INTO WL_DOCDAT_LOW.
    CONCATENATE S_DOCDAT-HIGH+6(2) '/' S_DOCDAT-HIGH+4(2) '/' S_DOCDAT-HIGH(4) INTO WL_DOCDAT_HIGH.
    CONCATENATE 'Período:' WL_DOCDAT_LOW 'até' WL_DOCDAT_HIGH INTO WL_TEXT SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_TEXT.
  ELSE.
    CONCATENATE S_DOCDAT-LOW+6(2)  '/' S_DOCDAT-LOW+4(2)  '/' S_DOCDAT-LOW(4)  INTO WL_DOCDAT_LOW.
    CONCATENATE 'Período:' WL_DOCDAT_LOW INTO WL_TEXT SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_TEXT.
  ENDIF.

ENDFORM.                    " INICIAR_VARIAVES
