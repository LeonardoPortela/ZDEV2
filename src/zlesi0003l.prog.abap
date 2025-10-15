*&---------------------------------------------------------------------*
*& Report  ZLESI0003L
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlesi0003l.

TYPE-POOLS: zlesi.

DATA: it_log       TYPE TABLE OF zlesi_log_processo INITIAL SIZE 0 WITH HEADER LINE,
      wa_log       TYPE zlesi_log_processo,
      cont         TYPE j_1bitmnum,
      vg_texto     TYPE string,
      v_posto_file TYPE zcodposto,
      wa_lfa1      TYPE lfa1,
      vg_posto     TYPE string,
      vg_cpf       TYPE c LENGTH 14,
      vg_cnpj      TYPE c LENGTH 18.

IMPORT it_log FROM MEMORY ID 'it_log'.
IMPORT v_posto_file FROM MEMORY ID 'v_posto_file'.

IF NOT v_posto_file IS INITIAL.
  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = v_posto_file
      p_partype    = 'V'
    CHANGING
      wa_info_part = wa_lfa1.
ENDIF.

cont = 1.

NEW-PAGE LINE-SIZE 160 LINE-COUNT 999.

ULINE.

IF NOT wa_lfa1 IS INITIAL.

  IF wa_lfa1-stkzn IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = wa_lfa1-stcd1
      IMPORTING
        output = vg_cnpj.
    CONCATENATE 'Posto:' wa_lfa1-name1 'CNPJ:' vg_cnpj INTO vg_posto SEPARATED BY space.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = wa_lfa1-stcd2
      IMPORTING
        output = vg_cpf.
    CONCATENATE 'Posto:' wa_lfa1-name1 'CPF:' vg_cpf INTO vg_posto SEPARATED BY space.
  ENDIF.

  WRITE: 01 vg_posto.
  ULINE.
ENDIF.

FORMAT COLOR COL_HEADING INTENSIFIED ON.
WRITE: 01 'Id', 08 'Texto'.
ULINE.

FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

LOOP AT it_log INTO wa_log.
  vg_texto = wa_log-texto.
  WRITE: /01 cont, 08 wa_log-texto.
  cont = cont + 1.
ENDLOOP.

ULINE.

TOP-OF-PAGE.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: 'Grupo André Maggi'.
