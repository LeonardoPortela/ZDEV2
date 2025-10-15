*&---------------------------------------------------------------------*
*& Report  ZMMR019_05
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR019_05.

FORM FATURA_ENTRADA_GRAOS  USING    P_VL_BSART    TYPE EKKO-BSART
                                    P_WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                                    P_WA_LGORT_CHARG TYPE CHARG_D
                           CHANGING P_WA_ITEMDATA_ITEM_TEXT TYPE SGTXT.

  IF P_VL_BSART = 'ZGR'.
    SELECT SINGLE MAKTX
      INTO @DATA(VMAKTX)
      FROM EKPO
      INNER JOIN MAKT
      ON   MAKT~MATNR = EKPO~MATNR
      AND  MAKT~SPRAS = 'P'
      WHERE EKPO~EBELN = @P_WA_MOV_ESTQ-PO_NUMBER
      AND   EKPO~EBELP = @P_WA_MOV_ESTQ-PO_ITEM.
    CONCATENATE 'Fatura Entrada Grãos' '-' VMAKTX+0(5) P_WA_LGORT_CHARG+0(4)  INTO P_WA_ITEMDATA_ITEM_TEXT SEPARATED BY SPACE.
  ENDIF.

ENDFORM.
