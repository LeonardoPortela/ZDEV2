*&---------------------------------------------------------------------*
*& Report  ZMMR132
*&
*&---------------------------------------------------------------------*
*&
*&JOB para marcar remessa final em pedidos com MIGO total entregue (ORDENS)
*&
*&Autor: Antonio Luiz Rodrigues da Silva
*&
*&Data: 07/02/2018
*&
*&---------------------------------------------------------------------*
REPORT zmmr132.

TYPES:
  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    menge TYPE ekbe-menge,
    dmbtr TYPE ekbe-dmbtr,
    shkzg TYPE ekbe-shkzg,
  END OF ty_ekbe.

DATA: t_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
DATA: t_item   LIKE bapimepoitem OCCURS 0 WITH HEADER LINE.
DATA: t_itemx  LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE.

DATA it_ekpo      TYPE TABLE OF ekpo.
DATA it_ekbe      TYPE TABLE OF ty_ekbe.
DATA it_ekbem     TYPE TABLE OF ty_ekbe.
DATA it_aufk     TYPE TABLE OF aufk.
DATA it_ekbe_soma  TYPE TABLE OF ty_ekbe.
DATA it_ekbe_somam TYPE TABLE OF ty_ekbe.
DATA wa_ekbe      TYPE ty_ekbe.
DATA wa_ekbem     TYPE ty_ekbe.
DATA vdatai       TYPE sy-datum.
DATA vdataf       TYPE sy-datum.
DATA vg_job       TYPE i.
DATA xv_jobnm TYPE btcjob.
DATA xv_stepc TYPE btcstepcnt.
DATA t_data                TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

PARAMETER: p_ebeln TYPE ekko-ebeln.


START-OF-SELECTION.


  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobname         = xv_jobnm
      stepcount       = xv_stepc
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.
  "
  IF xv_jobnm = 'MARCA_REMESSA_FINAL_MM'.
    SELECT SINGLE COUNT(*) INTO vg_job
      FROM tbtco
     WHERE jobname EQ 'MARCA_REMESSA_FINAL_MM'
       AND status EQ 'R'.
    IF ( vg_job EQ 1 ).
      PERFORM seleciona_dados_mm.
    ENDIF.
  ELSE.
    SELECT SINGLE COUNT(*) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'MARCA_REMESSA_FINAL_PM'
      AND status EQ 'R'.
    IF ( vg_job EQ 1 ).
      PERFORM seleciona_dados_pm.
    ENDIF.
  ENDIF.


FORM seleciona_dados_pm.
  REFRESH: it_ekpo, it_ekbe,it_ekbem, it_ekbe_soma, it_ekbe_somam.
  IF p_ebeln IS NOT INITIAL.
    SELECT *
     FROM ekpo
     INNER JOIN ekkn
      ON  ekkn~ebeln = ekpo~ebeln
      AND ekkn~ebelp = ekpo~ebelp
    INNER JOIN aufk
      ON  aufk~aufnr = ekkn~aufnr
      AND aufk~autyp = '30'
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    WHERE ekpo~ebeln EQ p_ebeln
    AND   ekpo~knttp EQ 'F'  "Ordem
    AND   ekpo~elikz EQ ''   "Remessa final desmarcada
    AND   ekpo~loekz EQ ''.
  ELSE.
    vdatai = sy-datum - 365.
    SELECT *
    FROM ekpo
    INNER JOIN ekkn
      ON  ekkn~ebeln = ekpo~ebeln
      AND ekkn~ebelp = ekpo~ebelp
    INNER JOIN aufk
      ON  aufk~aufnr = ekkn~aufnr
      AND aufk~autyp = '30'
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    WHERE ekpo~knttp EQ 'F'  "Ordem
    AND   ekpo~elikz EQ ''   "Remessa final desmarcada
    AND   ekpo~loekz EQ ''
    AND   ekpo~aedat GE vdatai.
  ENDIF.

  CHECK it_ekpo[] IS NOT INITIAL.

  SELECT *
     FROM ekbe
   INTO CORRESPONDING FIELDS OF TABLE it_ekbe "MIGO
  FOR ALL ENTRIES IN it_ekpo
  WHERE ebeln EQ it_ekpo-ebeln
  AND   ebelp EQ it_ekpo-ebelp
  AND   bewtp EQ 'E'.

  SELECT *
    FROM ekbe
  INTO CORRESPONDING FIELDS OF TABLE it_ekbem "MIRO
    FOR ALL ENTRIES IN it_ekpo
    WHERE ebeln EQ it_ekpo-ebeln
    AND   ebelp EQ it_ekpo-ebelp
    AND   bewtp EQ 'Q'.

  "MIGO
  LOOP AT it_ekbe INTO wa_ekbe.
    IF wa_ekbe-shkzg = 'H'.
      MULTIPLY  wa_ekbe-menge BY -1.
      MULTIPLY  wa_ekbe-dmbtr BY -1.
    ENDIF.
    CLEAR wa_ekbe-shkzg.
    COLLECT wa_ekbe INTO it_ekbe_soma.
  ENDLOOP.

  "MIRO
  LOOP AT it_ekbem INTO wa_ekbe.
    IF wa_ekbe-shkzg = 'H'.
      MULTIPLY  wa_ekbe-menge BY -1.
      MULTIPLY  wa_ekbe-dmbtr BY -1.
    ENDIF.
    CLEAR wa_ekbe-shkzg.
    COLLECT wa_ekbe INTO it_ekbe_somam.
  ENDLOOP.

  SORT it_ekbe_soma  BY ebeln ebelp.
  SORT it_ekbe_somam BY ebeln ebelp.


  LOOP AT it_ekpo INTO DATA(wa_ekpo).
    "MIGO
    READ TABLE it_ekbe_soma INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                                  ebelp = wa_ekpo-ebelp BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    "MIRO
    READ TABLE it_ekbe_somam INTO wa_ekbem WITH KEY ebeln = wa_ekpo-ebeln
                                                    ebelp = wa_ekpo-ebelp BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    "

    IF  ( ( wa_ekbe-menge GE wa_ekpo-menge ) AND ( wa_ekbe-menge EQ  wa_ekbem-menge ) AND wa_ekpo-pstyp NE 9 ) OR
        ( ( wa_ekbe-dmbtr GE wa_ekpo-brtwr ) AND ( wa_ekbe-dmbtr EQ  wa_ekbem-dmbtr ) AND wa_ekpo-pstyp EQ 9 ) .
      REFRESH: t_item, t_itemx,t_return.
      CLEAR: t_item, t_itemx,t_return.
      "
      t_item-po_item = wa_ekpo-ebelp.
*      T_ITEM-MATERIAL = WA_EKPO-MATNR.
*      T_ITEM-PLANT = WA_EKPO-WERKS.
*      T_ITEM-STGE_LOC = WA_EKPO-LGORT.
      t_item-no_more_gr = 'X'.
      APPEND t_item.

      t_itemx-po_item = wa_ekpo-ebelp.
      t_itemx-no_more_gr = 'X'.
      APPEND t_itemx.
*========================================Inicio BUG SOLTO 137647 / AOENNING.
*      CALL FUNCTION 'BAPI_PO_CHANGE'"#EC CI_USAGE_OK[2438131]
*        EXPORTING
*          PURCHASEORDER = WA_EKPO-EBELN
*        TABLES
*          RETURN        = T_RETURN
*          POITEM        = T_ITEM
*          POITEMX       = T_ITEMX.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = 'X'.
*
*      WAIT UP TO 1 SECONDS.
*      WRITE:/ 'Pedido->',WA_EKPO-EBELN, WA_EKPO-EBELP.
*      LOOP AT T_RETURN.
*        IF T_RETURN-TYPE = 'E'.
*          WRITE:/ T_RETURN-ID, T_RETURN-NUMBER, T_RETURN-MESSAGE.
*        ENDIF.
*      ENDLOOP.

      UPDATE  ekpo SET  elikz = 'X'
        WHERE  ebeln = wa_ekpo-ebeln
        AND    ebelp = wa_ekpo-ebelp.
      COMMIT WORK.

*========================================Fim bug solto 137647 / aoenning.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_MM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_mm.

  REFRESH: it_ekpo, it_ekbe, it_ekbe_soma.
  IF p_ebeln IS NOT INITIAL.
    SELECT *
     FROM ekpo
     INNER JOIN eket
      ON  eket~ebeln = ekpo~ebeln
      AND eket~ebelp = ekpo~ebelp
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    WHERE ekpo~ebeln EQ p_ebeln
    AND   ekpo~elikz EQ ''   "Remessa final desmarcada
    AND   ekpo~loekz EQ ''.
  ELSE.
    vdatai = sy-datum - 20000.
    vdataf = sy-datum - 120.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_REMESSA_MM'
      TABLES
        set_values    = t_data
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF t_data[] IS NOT INITIAL.
      READ TABLE t_data INDEX 1.
      vdatai = t_data-from.
      IF t_data-to LE vdataf.
        vdataf = t_data-to.
      ENDIF.
    ENDIF.


    SELECT *
    FROM ekpo
    INNER JOIN ekko
      ON  ekko~ebeln = ekpo~ebeln
    INNER JOIN eket
      ON  eket~ebeln = ekpo~ebeln
      AND eket~ebelp = ekpo~ebelp
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    WHERE ekpo~elikz EQ ''   "Remessa final desmarcada
    AND   ekpo~loekz EQ ''
    AND   ekpo~knttp NE 'F'  "Ordem
    AND   ekko~bsart IN ('PCS','PCSI','PSEF''ZNB','ZDBP', 'ZEF', 'REG' )
    AND   eket~eindt BETWEEN vdatai AND vdataf. "mais de 365 dias da data de remessa e pendente
*    AND   EKET~EINDT LE VDATAI. "mais de 365 dias da data de remessa e pendente
  ENDIF.

  CHECK it_ekpo[] IS NOT INITIAL.

  SELECT *
     FROM ekbe
   INTO CORRESPONDING FIELDS OF TABLE it_ekbe "MIGO
  FOR ALL ENTRIES IN it_ekpo
  WHERE ebeln EQ it_ekpo-ebeln
  AND   ebelp EQ it_ekpo-ebelp
  AND   bewtp EQ 'E'.


  "MIGO
  LOOP AT it_ekbe INTO wa_ekbe.
    IF wa_ekbe-shkzg = 'H'.
      MULTIPLY  wa_ekbe-menge BY -1.
      MULTIPLY  wa_ekbe-dmbtr BY -1.
    ENDIF.
    CLEAR wa_ekbe-shkzg.
    COLLECT wa_ekbe INTO it_ekbe_soma.
  ENDLOOP.


  SORT it_ekbe_soma  BY ebeln ebelp.

  LOOP AT it_ekpo INTO DATA(wa_ekpo).
    "MIGO
    CLEAR wa_ekbe.
    READ TABLE it_ekbe_soma INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                                  ebelp = wa_ekpo-ebelp BINARY SEARCH.

    " Pendentes
    IF  ( ( wa_ekbe-menge LT wa_ekpo-menge ) AND wa_ekpo-pstyp NE 9 ) OR
        ( ( wa_ekbe-dmbtr LT wa_ekpo-brtwr ) AND wa_ekpo-pstyp EQ 9 ) .
      REFRESH: t_item, t_itemx,t_return.
      CLEAR: t_item, t_itemx,t_return.
      t_item-po_item = wa_ekpo-ebelp.
*      T_ITEM-MATERIAL = WA_EKPO-MATNR.
*      T_ITEM-PLANT = WA_EKPO-WERKS.
*      T_ITEM-STGE_LOC = WA_EKPO-LGORT.
      t_item-no_more_gr = 'X'.
      APPEND t_item.

      t_itemx-po_item = wa_ekpo-ebelp.
      t_itemx-no_more_gr = 'X'.
      APPEND t_itemx.


      CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          purchaseorder = wa_ekpo-ebeln
        TABLES
          return        = t_return
          poitem        = t_item
          poitemx       = t_itemx.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      WAIT UP TO 1 SECONDS.

      WRITE:/ 'Pedido->',wa_ekpo-ebeln, wa_ekpo-ebelp.
      LOOP AT t_return.
        IF t_return-type = 'E'.
          WRITE:/ t_return-id, t_return-number, t_return-message.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.
