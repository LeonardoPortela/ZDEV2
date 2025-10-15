*&---------------------------------------------------------------------*
*& Include          ZMMR196_EVT
*&---------------------------------------------------------------------*


START-OF-SELECTION.

  DATA: lr_acess(1),
        lr_werks TYPE mseg-werks.

  LOOP AT  s_werks INTO DATA(w_werks).
    lr_werks = s_werks-low.

    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
           ID 'WERKS' FIELD  lr_werks
           ID 'ACTVT' FIELD '03'.    "Alteração

    IF w_werks-high IS NOT INITIAL AND sy-subrc = 0.
      lr_werks = w_werks-high.
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
          ID 'WERKS' FIELD  lr_werks
          ID 'ACTVT' FIELD '03'.    "Alteração
    ENDIF.

    CASE sy-subrc.
      WHEN 0.
        "Tem autorização!
      WHEN 4.
        lr_acess = 'X'.
        MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
        SET CURSOR FIELD 'S_WERKS'.
      WHEN 12.
        lr_acess = 'X'.
        MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
        SET CURSOR FIELD 'S_WERKS'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF lr_acess = 'X'.
    EXIT.
  ENDIF.

  PERFORM f_dispara_md01.
  PERFORM f_seleciona_dados.
  PERFORM f_processa_dados.
  PERFORM f_exibe_alv.
