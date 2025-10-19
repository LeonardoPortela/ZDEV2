FUNCTION Z_SD_INFO_NFE_EXPORTACAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(P_TELA) TYPE  CHAR01 DEFAULT 'x'
*"     VALUE(P_ZNFECOMEX) TYPE  ZNFECOMEX OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ZNFECOMEX) TYPE  ZNFECOMEX
*"  EXCEPTIONS
*"      NAO_LOCALIZADO
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
  DATA: WA_ZNFECOMEX      TYPE ZNFECOMEX,
        WA_FATURA_SERVICO TYPE VBRP,
        IT_NFE_ITEM       TYPE TABLE OF J_1BNFLIN,
        WA_NFE_ITEM       TYPE J_1BNFLIN,
        I_XVBADR          TYPE TABLE OF SADRVB,
        I_XVBPA           TYPE TABLE OF VBPAVB,
        WA_INFO_PART      TYPE LFA1,
        P_PARCEIRO        TYPE J_1BPARID,
        WA_INFO_C         TYPE KNA1,
        SL_XVBPA          TYPE VBPAVB,
        WA_ZFIWRT0015     TYPE ZFIWRT0015.

*"----------------------------------------------------------------------
*" Tabela: Tab. Cabeçalho de Info. de Comércio Exterior
*"----------------------------------------------------------------------
*  SELECT SINGLE *
*    FROM ZNFECOMEX
*    INTO WA_ZNFECOMEX
*  WHERE DOCNUM EQ P_DOCNUM.

*  IF SY-SUBRC IS INITIAL.
*    MOVE WA_ZNFECOMEX TO P_ZNFECOMEX.
*  ELSEIF P_TELA IS NOT INITIAL.

  "WA_ZNFECOMEX-DOCNUM = P_DOCNUM.

  SELECT * INTO TABLE IT_NFE_ITEM
    FROM J_1BNFLIN
   WHERE DOCNUM EQ P_DOCNUM.

  CHECK SY-SUBRC IS INITIAL.

  READ TABLE IT_NFE_ITEM INTO WA_NFE_ITEM INDEX 1.

  IF WA_NFE_ITEM-REFTYP EQ 'BI'.

    "Fatura do Serviço
    SELECT SINGLE * INTO WA_FATURA_SERVICO
      FROM VBRP
     WHERE VBELN = WA_NFE_ITEM-REFKEY(10)
       AND POSNR = WA_NFE_ITEM-REFITM.

    CHECK SY-SUBRC IS INITIAL.

    CALL FUNCTION 'SD_PARTNER_READ'
      EXPORTING
        F_VBELN  = WA_FATURA_SERVICO-AUBEL
        OBJECT   = 'VBPA'
      TABLES
        I_XVBADR = I_XVBADR
        I_XVBPA  = I_XVBPA.

    DELETE I_XVBPA WHERE PARVW NE 'Z1'.

    IF I_XVBPA[] IS NOT INITIAL.

      READ TABLE I_XVBPA[] INTO SL_XVBPA INDEX 1.

      IF NOT SL_XVBPA-LIFNR IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = SL_XVBPA-LIFNR
          IMPORTING
            OUTPUT = P_PARCEIRO.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = P_PARCEIRO
            P_PARTYPE    = 'V'
          CHANGING
            WA_INFO_PART = WA_INFO_PART.

        E_ZNFECOMEX-UFEMBARQ   = WA_INFO_PART-TXJCD(2).
        E_ZNFECOMEX-XLOCEMBARQ = WA_INFO_PART-ORT01.
        E_ZNFECOMEX-NAME1      = WA_INFO_PART-NAME1.

      ELSEIF NOT SL_XVBPA-KUNNR IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = SL_XVBPA-KUNNR
          IMPORTING
            OUTPUT = P_PARCEIRO.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO = P_PARCEIRO
            P_PARTYPE  = 'C'
          CHANGING
            WA_INFO_C  = WA_INFO_C.

        E_ZNFECOMEX-UFEMBARQ   = WA_INFO_C-TXJCD(2).
        E_ZNFECOMEX-XLOCEMBARQ = WA_INFO_C-ORT01.

      ENDIF.
    ENDIF.

  ELSEIF WA_NFE_ITEM-REFTYP EQ 'ZW'.

    SELECT SINGLE * INTO WA_ZFIWRT0015
      FROM ZFIWRT0015
        WHERE SEQ_LCTO = WA_NFE_ITEM-REFKEY(10)
        AND PARVW    = 'Z1'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZFIWRT0015-PARID
      IMPORTING
        OUTPUT = P_PARCEIRO.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        P_PARCEIRO   = P_PARCEIRO
        P_PARTYPE    = 'V'
      CHANGING
        WA_INFO_PART = WA_INFO_PART.

    E_ZNFECOMEX-UFEMBARQ   = WA_INFO_PART-TXJCD(2).
    E_ZNFECOMEX-XLOCEMBARQ = WA_INFO_PART-ORT01.
    E_ZNFECOMEX-NAME1      = WA_INFO_PART-NAME1.

  ENDIF.

*  ELSEIF P_TELA IS INITIAL.
*    MESSAGE E023 WITH 'Não localizado inf. comercio exterior!' RAISING NAO_LOCALIZADO.
*  ENDIF.

*----------------------------------------------------------------------*
* CALL SCREEN
*----------------------------------------------------------------------*
*  IF P_TELA IS NOT INITIAL.
*    CALL SCREEN 9980 STARTING AT 07 05 ENDING AT 115 03.
*  ENDIF.

  "MOVE WA_ZNFECOMEX TO P_ZNFECOMEX.

ENDFUNCTION.
