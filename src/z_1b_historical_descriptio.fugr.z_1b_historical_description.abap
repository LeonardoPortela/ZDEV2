FUNCTION Z_1B_HISTORICAL_DESCRIPTION.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LINE_BSEG) TYPE  BSEG
*"     REFERENCE(LINE_BKPF) TYPE  BKPF
*"  EXPORTING
*"     REFERENCE(E_SGTXT) TYPE  CHAR50
*"----------------------------------------------------------------------
* Função criada conforme consta na nota 108559 ref. *Historico Padrão*
  TABLES:
    VBRP, VBAK, TVAKT, KNA1, LFA1, SKAT, T001, MSEG, T156T, T003T.

  DATA:
    HLP_TEXT(100)    TYPE C,
    ACCOUNT_NAME(20) TYPE C.

  E_SGTXT = LINE_BSEG-SGTXT.
  "NÃO TROCA O TEXTO DA INTERFACE DE GRÃOS SE JA ESTIVER PREENCHIDO NO FORNECEDOR
*--------User Story 115809 / CS2023000488 / AOENNING / BSCHL = '21'
  IF LINE_BKPF-BLART = 'ZG' AND ( LINE_BSEG-BSCHL = '31' OR  LINE_BSEG-BSCHL = '21' ) AND E_SGTXT IS  INITIAL AND SY-CPROG = 'ZMMR019'.
    IMPORT P1 = E_SGTXT FROM MEMORY ID 'MZMMR019'.
    IF  E_SGTXT IS NOT INITIAL.
      FREE MEMORY ID 'MZMMR019'.
      EXIT.
    ENDIF.
  ENDIF.

  FREE MEMORY ID 'MZMMR019'.

* Text empty ?
*  check line_bseg-sgtxt = space.

  CLEAR E_SGTXT.

* Document type
  IF LINE_BKPF-BUKRS = '0100' OR LINE_BKPF-BUKRS = '0101'.
    SELECT SINGLE *
    FROM T003T
   WHERE SPRAS = 'S'
     AND BLART = LINE_BKPF-BLART.
  ELSE.
    SELECT SINGLE *
      FROM T003T
     WHERE SPRAS = SY-LANGU
       AND BLART = LINE_BKPF-BLART.
  ENDIF.

  E_SGTXT(20) = T003T-LTEXT.

  CASE LINE_BSEG-KOART.
    WHEN 'D'. " Quando Cliente
      SELECT SINGLE *
        FROM KNA1
       WHERE KUNNR = LINE_BSEG-KUNNR.
      ACCOUNT_NAME = KNA1-NAME1.
      TRANSLATE ACCOUNT_NAME TO UPPER CASE.

    WHEN 'K'. " Quando fornecedor
      SELECT SINGLE *
        FROM LFA1
       WHERE LIFNR = LINE_BSEG-LIFNR.
      ACCOUNT_NAME = LFA1-NAME1.
      TRANSLATE ACCOUNT_NAME TO UPPER CASE.

    WHEN OTHERS.
      SELECT SINGLE *
        FROM T001
       WHERE BUKRS = LINE_BSEG-BUKRS.

      IF LINE_BKPF-BUKRS = '0100' OR LINE_BKPF-BUKRS = '0101'.
        SELECT SINGLE *
          FROM SKAT
         WHERE SPRAS = 'S'
           AND KTOPL = T001-KTOPL
           AND SAKNR = LINE_BSEG-HKONT.
      ELSE.
        SELECT SINGLE *
          FROM SKAT
         WHERE SPRAS = SY-LANGU
           AND KTOPL = T001-KTOPL
           AND SAKNR = LINE_BSEG-HKONT.
      ENDIF.

      ACCOUNT_NAME = SKAT-TXT20.

  ENDCASE.

  HLP_TEXT   = E_SGTXT.
  CONCATENATE HLP_TEXT ACCOUNT_NAME INTO E_SGTXT SEPARATED BY ' '.

  HLP_TEXT    = E_SGTXT.
*   text-001    = free choice, but should be no longer than 10
*  concatenate hlp_text text-001 into e_sgtxt separated by ' '. sem text-001 conforme solicitado

ENDFUNCTION.
