*&---------------------------------------------------------------------*
*&  Include           ZXM02U08
*&---------------------------------------------------------------------*



DATA: t_esll          TYPE esll,
      t_zmm_aprov_rcc TYPE zmm_aprov_rcc,
      w_eban          TYPE eban,
      w_eban2         TYPE eban,
      w_eban3         TYPE eban,
      w_ebkn          TYPE ebkn,
      vl_erro(1).


DATA: BEGIN OF ls_ekpo,
*        include structure ekpo.
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        menge TYPE ekpo-menge,
        loekz TYPE ekpo-loekz,
      END OF ls_ekpo.

DATA: global_framework TYPE REF TO cl_framework_mm.

CONSTANTS: trans(5)    TYPE c VALUE 'ME52N',
           c_me22n(5)  TYPE c VALUE 'ME22N',
           c_x         TYPE c VALUE 'X',
           txt_01(100) TYPE c VALUE 'Item da requisição de compras não pode ser ',
           txt_02(100) TYPE c VALUE 'modificado, pedido de compras ativo.'.

FIELD-SYMBOLS <eban> TYPE ueban.


*** Victor Hugo - 22.02.2012 - Bloqueio alterações requisições compra após pedido criado - Inicio
IF  ( ( sy-tcode = 'ME51N' OR sy-tcode = 'ME52N' OR sy-tcode = 'ME53N' ) OR ( sy-tcode EQ c_me22n ) AND ( sy-ucomm EQ 'MESAVE' OR sy-ucomm EQ 'YES' ) ) .

  "grava treinamento rc
*  CALL FUNCTION 'Z_MM_TREINA_COMPRAS'
*    EXPORTING
*      i_ebeln = im_banfn_new
*      i_ebelp = '0010'
*      i_btn   = ' '
*      i_tipo  = 'R'
*    IMPORTING
*      i_erro  = vl_erro.

  CLEAR: ls_ekpo.
  LOOP AT im_eban_changes INTO w_eban.
    MOVE-CORRESPONDING w_eban TO w_eban3. "alterações change
    SELECT SINGLE *
         FROM eban
         INTO w_eban2 "alterações table fisica
         WHERE banfn = im_banfn_new
         AND   bnfpo = w_eban-bnfpo.

    CLEAR: w_eban2-ekgrp, w_eban3-ekgrp. "Permite alterar somente o comprador
    CLEAR: w_eban2-txz01, w_eban3-txz01. "Permite alterar texto
    CLEAR: w_eban2-ernam, w_eban3-ernam.
    CLEAR: w_eban2-erdat, w_eban3-erdat.
    CLEAR: w_eban2-frgzu, w_eban3-frgzu.
    CLEAR: w_eban2-frgkz, w_eban3-frgkz.
    CLEAR: w_eban2-creationdate, w_eban3-creationdate.
    CLEAR: w_eban2-creationtime, w_eban3-creationtime.
    IF w_eban3 EQ  w_eban2 OR w_eban-ebakz = 'X'. "não houve alteração
      CONTINUE.
    ENDIF.

    SELECT SINGLE ebeln ebelp menge loekz FROM ekpo
      INTO ls_ekpo
    WHERE ebeln EQ w_eban-ebeln
      AND ebelp EQ w_eban-ebelp.


    IF NOT ( ls_ekpo-ebeln IS INITIAL ).

      IF ( ( ls_ekpo-loekz EQ '' ) AND ( ls_ekpo-loekz NE 'L' ) ).
        "message e888(sabapdocu) with txt_01 txt_02 raising failed.

        CALL METHOD cl_framework_mm=>get_instance
          IMPORTING
            ex_instance = global_framework.

        IF sy-ucomm EQ 'YES'.
          CALL METHOD global_framework->set_fcode
            EXPORTING
              im_fcode = 'NO'.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH txt_01 txt_02 'O doc. não foi salvo !'.
          LEAVE TO SCREEN 0.
        ELSE.
          CALL METHOD global_framework->set_fcode
            EXPORTING
              im_fcode = space.
        ENDIF.
        MESSAGE e888(sabapdocu) WITH txt_01 txt_02.
      ELSEIF ( ls_ekpo-loekz EQ c_x ).
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDIF.
*** Victor Hugo - 22.02.2012 - Bloqueio alterações requisições compra após pedido criado - FIM


*** Victor Hugo - 02.03.2012 - Bloqueio requisições sem código material - Inicio
CONSTANTS: c_me51n(5) TYPE c VALUE 'ME51N',
           c_me52n(5) TYPE c VALUE 'ME52N',
           c_me53n(5) TYPE c VALUE 'ME53N'.


IF  im_banfn_new = im_banfn_old AND sy-tcode NE 'ME54N' AND sy-batch NE 'X'. "alteração
  LOOP AT im_eban_changes INTO w_eban.
    IF w_eban-ebakz EQ 'X'.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING w_eban TO w_eban3. "alterações change
    SELECT SINGLE *
         FROM eban
         INTO w_eban2 "alterações table fisica
         WHERE banfn = im_banfn_new
         AND   bnfpo = w_eban-bnfpo.

    CLEAR: w_eban2-ekgrp, w_eban3-ekgrp. "Permite alterar somente o comprador
    CLEAR: w_eban2-txz01, w_eban3-txz01. "Permite alterar texto
    CLEAR: w_eban2-ernam, w_eban3-ernam.
    CLEAR: w_eban2-erdat, w_eban3-erdat.

    IF w_eban3-menge EQ  w_eban2-menge and
       w_eban3-matnr EQ  w_eban2-matnr and
       w_eban3-loekz EQ  w_eban2-loekz and
       w_eban3-ebakz EQ  w_eban2-ebakz  ."não houve alteração
      CONTINUE.
    ENDIF.


    IF w_eban-loekz EQ '' AND w_eban-ebakz EQ ''.
      SELECT SINGLE *
         FROM eban
         INTO w_eban2
         WHERE banfn = im_banfn_new
         AND   bnfpo = w_eban-bnfpo
         AND   frgkz = '2'.
      IF sy-subrc = 0.
*-IR038581 - JAime Tassoni - descomentar - 05.11.2020 - inicio
        MESSAGE e000(z01)
                WITH 'RC aprovada, não sendo possível alterar. ' w_eban-bnfpo.
*-IR038581 - JAime Tassoni - descomentar - 05.11.2020 - fim
      ELSE.
        SELECT SINGLE *
          FROM eban
          INTO w_eban2
          WHERE banfn = im_banfn_new
          AND   bnfpo = w_eban-bnfpo.
        IF sy-subrc NE 0. "novo item
          SELECT SINGLE *
             FROM eban
             INTO w_eban2
             WHERE banfn = im_banfn_new
             AND   frgkz = '2'.
          IF sy-subrc = 0.
*            MESSAGE E000(Z01)
*                    WITH 'RC aprovada, não sendo possível incluir. '.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDIF.


IF ( ( sy-tcode EQ c_me51n ) OR ( sy-tcode EQ c_me52n ) AND ( sy-ucomm EQ 'MESAVE' ) OR ( sy-ucomm EQ 'YES' ) ).
*  "IVA
*  LOOP AT IM_EBAN_CHANGES ASSIGNING <EBAN>.
*    <EBAN>-ZDESTI = '2'.
*  ENDLOOP.

  CLEAR: w_eban.

  LOOP AT im_eban_changes INTO w_eban.

    IF ( ( w_eban-matnr EQ '' ) AND ( w_eban-pstyp NE '9') ).


      CALL METHOD cl_framework_mm=>get_instance
        IMPORTING
          ex_instance = global_framework.

      IF ( sy-ucomm EQ 'YES' ).
        CALL METHOD global_framework->set_fcode
          EXPORTING
            im_fcode = 'NO'.


        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'O documento não foi salvo!'.
        LEAVE TO SCREEN 0.

      ELSE.
        CALL METHOD global_framework->set_fcode
          EXPORTING
            im_fcode = space.
      ENDIF.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = w_eban-bnfpo
        IMPORTING
          output = w_eban-bnfpo.



      MESSAGE e000(z01) WITH 'Por favor informar o código do material do item' w_eban-bnfpo.


    ENDIF.
    CLEAR: w_eban.
  ENDLOOP.
ENDIF.
