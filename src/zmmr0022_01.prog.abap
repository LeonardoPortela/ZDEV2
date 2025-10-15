*----------------------------------------------------------------------*
***INCLUDE ZMMR0022_01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LIMPA_EKBE_MOVI_ENVERSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKBE  text
*----------------------------------------------------------------------*
*FORM LIMPA_EKBE_MOVI_ENVERSO  TABLES IT_EKBE_IN STRUCTURE TY_EKBE.
*
*  DATA: IT_EKBE_AUX TYPE TABLE OF TY_EKBE,
*        WA_EKBE     TYPE TY_EKBE,
*        WA_EKBE_AUX TYPE TY_EKBE.
*
*  MOVE IT_EKBE_IN TO IT_EKBE_AUX.
*
*  "Percorrer Entrada verificando Saídas
*  LOOP AT IT_EKBE_AUX INTO WA_EKBE_AUX WHERE SHKZG EQ 'S'.
*
*    "Procura Lançamento Inverso
*    READ TABLE IT_EKBE_IN INTO WA_EKBE
*      WITH KEY EBELN = WA_EKBE_AUX-EBELN
*               EBELP = WA_EKBE_AUX-EBELP
*               ZEKKN = WA_EKBE_AUX-ZEKKN
*               VGABE = WA_EKBE_AUX-VGABE
*               GJAHR = WA_EKBE_AUX-GJAHR
*               XBLNR = WA_EKBE_AUX-XBLNR
*               SHKZG = 'H'.
*
*  ENDLOOP.
*
*ENDFORM.                    " LIMPA_EKBE_MOVI_ENVERSO
