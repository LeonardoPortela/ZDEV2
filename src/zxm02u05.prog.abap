*&---------------------------------------------------------------------*
*&  Include           ZXM02U05
*&---------------------------------------------------------------------*

*** Modificação - Eduardo Ruttkowski 0 27/03/2008 >>> INICIO.
DATA: messages LIKE ex_messages WITH HEADER LINE,
      w_ebeln  LIKE ekko-ebeln,
      w_ebelp  LIKE ekpo-ebelp,
      s_bukrs  TYPE bukrs,
      wl_loekz TYPE ekpo-loekz.

FIELD-SYMBOLS: <banfn> TYPE any.

ASSIGN ('(SAPLMEGUI)MEREQ_TOPLINE-BANFN_EXT') TO <banfn>.

IF sy-tcode EQ 'ME52N' OR sy-tcode EQ 'ME53N' OR sy-tcode EQ 'ME54N'.
  IF sy-subrc EQ 0.
    CLEAR:  s_bukrs.
    SELECT SINGLE vkorg
    FROM t001w
    INTO s_bukrs
    WHERE spras EQ 'P'
    AND werks EQ im_data_new-werks.

    "Verifica se existe empresa do pedido no SET.
    SELECT SINGLE *
     FROM setleaf
     INTO @DATA(i_data)
       WHERE setname EQ 'MAGGI_EMPRESAS_COUPA'
         AND valfrom EQ @s_bukrs.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM eban INTO @DATA(w_eban) WHERE banfn EQ @<banfn> AND bnfpo EQ @im_data_new-bnfpo AND status_coupa EQ 'S'.
      IF sy-subrc EQ 0.
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Requisição criada pelo COUPA, '.
        messages-message_v2 = 'não é possivel modificar, '.
        messages-message_v3 = 'item:'.
        messages-message_v4 = im_data_new-bnfpo.
        APPEND messages TO ex_messages.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR: w_eban.

IF im_data_new-loekz = space.
  IF im_data_new-knttp = 'A'.   "Quando requisiçao para ativo
    IF im_data_new-menge > 1000.
      CLEAR ex_messages.
      messages-type = 'E'.
      messages-id = 'Z01'.
      messages-number = '000'.
      messages-message_v1 = 'Quantidade informada deve '.
      messages-message_v2 = 'ser 1 se for maior inserir nova  '.
      messages-message_v3 = 'linha nesta requisição - Item:'.
      messages-message_v4 = im_data_new-bnfpo.
      APPEND messages TO ex_messages.
    ENDIF.
  ENDIF.
ENDIF.

** ALRS
IF sy-tcode EQ 'ME51N'
OR sy-tcode EQ 'ME52N'
OR sy-tcode EQ 'ME53N'.
  FIELD-SYMBOLS: <fs_desti> TYPE any.
  CONSTANTS: c_desti(28) VALUE 'CI_EBANDB-ZDESTI'.
  ASSIGN (c_desti) TO <fs_desti>.

ENDIF.

** Igor Vilela - 01.08.2011 - Validação de itens para exclusao - inicio
IF sy-tcode EQ 'ME52N' OR sy-tcode EQ 'ME53N' OR sy-tcode EQ 'ME54N'.
  IF im_data_new-loekz = 'X'.
    IF  im_data_new-ebeln IS NOT INITIAL
    AND im_data_new-ebelp IS NOT INITIAL.

      SELECT SINGLE loekz
        FROM ekpo
        INTO wl_loekz
         WHERE ebeln EQ im_data_new-ebeln
           AND ebelp EQ im_data_new-ebelp.
      IF wl_loekz EQ space.
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Item da requisição de '.
        messages-message_v2 = 'compras não pode ser excluído,'.
        messages-message_v3 = 'pedido de compra ativo.'.
        APPEND messages TO ex_messages.
      ENDIF.

    ENDIF.
  ENDIF.


  IF  im_data_new-ebeln IS NOT INITIAL
  AND im_data_new-ebelp IS NOT INITIAL.

    SELECT SINGLE *
      FROM ekpo
      INTO @DATA(w_ekpo)
       WHERE ebeln EQ @im_data_new-ebeln
         AND ebelp EQ @im_data_new-ebelp.

    IF sy-subrc = 0.
      IF w_ekpo-loekz NE 'L'.
        IF  im_data_new-ebakz EQ im_data_old-ebakz ."CS2021001079 se for marcar/desmarcar concluido permitir
          CLEAR: w_eban.
          SELECT SINGLE *
            FROM eban
            INTO @w_eban
            WHERE banfn = @w_ekpo-banfn
            AND   bnfpo = @w_ekpo-bnfpo.
          IF w_eban-frgkz = '2'.
            CLEAR ex_messages.
            messages-type = 'E'.
            messages-id = 'Z01'.
            messages-number = '000'.
            messages-message_v1 = 'Item da requisição de '.
            messages-message_v2 = 'compras não pode ser modificado,'.
            messages-message_v3 = 'pedido de compra ativo.'.
            APPEND messages TO ex_messages.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

ENDIF.

** Igor Vilela - 01.08.2011 - Validação de itens para exclusao - fim
IF im_data_new-knttp NE space.
  "CS2020000037 Bloqueio PC CC EPI
  SELECT SINGLE *
    FROM mara
    INTO @DATA(wmara)
    WHERE matnr = @im_data_new-matnr
    AND   mtart = 'ZEPI'.
  IF sy-subrc = 0.
    CLEAR ex_messages.
    messages-type = 'E'.
    messages-id = 'Z01'.
    messages-number = '000'.
    messages-message_v1 = 'Material de EPI.'.
    messages-message_v2 = 'Somente pode ser comprado para estoque'.

    APPEND messages TO ex_messages.
  ENDIF.
ENDIF.
IF sy-tcode EQ 'ME51N'
OR sy-tcode EQ 'ME52N'.
  DATA: wl_0040  TYPE zmmt0040,
        wl_0048  TYPE zmmt0048,
        wl_mard  TYPE mard,
        wl_lgort TYPE mard-lgort.
  CLEAR: wl_mard, wl_lgort.

** Modificação - Eduardo Ruttkowski Tavares - 17.10.2013 >>> INI
* CH 107878 - ZXM02U05
  IF im_data_new-knttp NE space.
    SELECT SINGLE *
      FROM  mard
        INTO wl_mard
         WHERE matnr EQ im_data_new-matnr
           AND werks EQ im_data_new-werks.

    wl_lgort = wl_mard-lgort.
    SELECT SINGLE *
      FROM zmmt0048
       INTO wl_0048
        WHERE werks EQ im_data_new-werks
          AND matnr EQ im_data_new-matnr
          AND lgort EQ im_data_new-lgort. "WL_LGORT.

    IF sy-subrc IS NOT INITIAL.
      IF wl_mard-lgort IS NOT INITIAL AND im_data_new-lgort IS NOT INITIAL.
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Material bloqueado para aquisição em custo direto.'.
        messages-message_v2 = 'Por favor, procure o almoxarifado.'.

        APPEND messages TO ex_messages.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE *
         FROM  mard
           INTO wl_mard
            WHERE matnr EQ im_data_new-matnr
              AND werks EQ im_data_new-werks.

    wl_lgort = wl_mard-lgort.
    SELECT SINGLE *
      FROM zmmt0048
       INTO wl_0048
        WHERE werks EQ im_data_new-werks
          AND matnr EQ im_data_new-matnr
          AND lgort EQ im_data_new-lgort. "WL_LGORT.

    IF sy-subrc IS INITIAL.
      IF wl_mard-lgort IS NOT INITIAL AND wl_0048-ambos EQ 'X'.
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Material liberado apenas para Custo Direto'.
        messages-message_v2 = 'Por favor, procure o almoxarifado.'.

        APPEND messages TO ex_messages.
      ENDIF.
    ENDIF.
  ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 17.10.2013 <<< END


  SELECT SINGLE *
    FROM zmmt0040
    INTO wl_0040
     WHERE usnam EQ sy-uname.
  IF sy-subrc IS NOT INITIAL.
    IF im_data_new-knttp = space.
      CLEAR ex_messages.
      messages-type = 'E'.
      messages-id = 'Z01'.
      messages-number = '000'.
      messages-message_v1 = 'O usuário não tem permissão para'.
      messages-message_v2 = 'lançar requisição de Estoque.'.
*        MESSAGES-MESSAGE_V3 = 'pedido de compra ativo.'.
      APPEND messages TO ex_messages.


    ENDIF.
  ENDIF.
ENDIF.

DATA: wa_marc TYPE marc,
      wa_mard TYPE mard.

DATA: soma TYPE eban-menge.

CLEAR: soma.

" Validação para Estoque minimo e maximo para compras.
IF ( sy-tcode EQ 'ME51N' ) OR ( sy-tcode EQ 'ME52N' ) OR ( sy-tcode EQ 'ME53N' ).

  IF ( im_data_new-loekz NE 'X' ).
    IF im_data_new-pstyp EQ '9' .
      IF im_data_new-matnr IS INITIAL.
        MESSAGE e000(z01) WITH 'R.C. de serviço deve ser asocciada a um material'
                              'Contacte o SUPRIMENTOS'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( im_data_new-knttp IS INITIAL ).

    IF ( im_data_new-loekz NE 'X' ).
      " Validação do Estoque Minimo.
      SELECT SINGLE * FROM marc INTO wa_marc WHERE matnr EQ im_data_new-matnr
                                               AND werks EQ im_data_new-werks.

      IF  NOT ( wa_marc-bstmi IS INITIAL ).
        IF (  im_data_new-menge < wa_marc-bstmi ).
          MESSAGE e000(z01) WITH 'Compra abaixo da quantidade' ' mínima para este material.'.
        ENDIF.
      ENDIF.

      CLEAR: wa_marc.

      " Validação do Estoque Maximo.
      SELECT SINGLE * FROM marc INTO wa_marc WHERE matnr EQ im_data_new-matnr
                                               AND werks EQ im_data_new-werks.

      IF ( sy-subrc EQ 0 ) AND NOT ( wa_marc-mabst IS INITIAL ).


        SELECT SINGLE * FROM mard INTO wa_mard WHERE matnr EQ wa_marc-matnr
                                                 AND werks EQ wa_marc-werks.

        soma = wa_mard-labst + im_data_new-menge.


        IF ( soma > wa_marc-mabst ).
          MESSAGE e000(z01) WITH  'Compra acima do estoque máximo' ' permitido para esse material'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
*
*  "ALRS 27/01/2017 - CS2016001296
*  FIELD-SYMBOLS: <FS_ESLL> TYPE TABLE.
*  CONSTANTS: C_ESLL(28) VALUE '(SAPLMLSP)IX_ESLL[]'.
*
*  "Checa se texto é diferente do serviço
*  DATA: T_IX_ESLL   TYPE STANDARD TABLE OF ZIX_ESLL,
*        WA_IX_ESLL1 TYPE ZIX_ESLL,
*        WA_IX_ESLL2 TYPE ZIX_ESLL.
*
*  IF IM_DATA_NEW-PSTYP EQ '9'.
*    ASSIGN (C_ESLL) TO <FS_ESLL>.
*    IF <FS_ESLL> IS ASSIGNED.
*      T_IX_ESLL[] = <FS_ESLL>.
*      IF T_IX_ESLL[] IS NOT INITIAL.
*        READ TABLE T_IX_ESLL  INTO WA_IX_ESLL1 WITH KEY MSUPDAP-PACKNO = IM_DATA_NEW-PACKNO.
*        IF SY-SUBRC = 0.
*          READ TABLE T_IX_ESLL  INTO WA_IX_ESLL2 WITH KEY MSUPDAP-PACKNO = WA_IX_ESLL1-MSUPDAP-SUB_PACKNO.
*          IF SY-SUBRC = 0.
*            IF WA_IX_ESLL2-MSUPDAP-KTEXT1 NE IM_DATA_NEW-TXZ01.
*              MESSAGE E000(Z01) WITH 'Texto do item é diferente'
*                                     'do texto do serviço linha' IM_DATA_NEW-BNFPO .
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDIF.


**** Modificação - Eduardo Ruttkowski 0 27/03/2008 <<< FIM.

*** Inclusão - Armando Dyna 31/03/2008 >>> INICIO.
SELECT SINGLE ebeln FROM ekko
  INTO w_ebeln
  WHERE ebeln = im_data_new-konnr AND
        bstyp = 'K'               AND
        kdatb = sy-datum          AND
        kdate = sy-datum.

IF sy-subrc = 0.
  SELECT SINGLE ebelp FROM ekpo
    INTO w_ebelp
    WHERE ebeln = w_ebeln AND
          matnr = im_data_new-matnr AND
          werks = im_data_new-werks AND
          loekz = space.

  IF sy-subrc = 0.
*    IM_DATA_NEW-KTPNR = W_EBELP.
  ENDIF.

ENDIF.
*** Inclusão - Armando Dyna 31/03/2008 >>> FIM.

*** Stefanini - IR240290 - 24/06/2025 - FINC - Início de Alteração
"Apenas na eliminação do item
IF ( sy-tcode EQ 'ME51N' OR sy-tcode EQ 'ME52N' OR sy-tcode EQ 'ME53N' ) AND im_data_new-loekz = 'X' AND im_data_old-loekz IS INITIAL.

  SELECT SINGLE mandt
    INTO @sy-mandt
    FROM eban
   WHERE banfn    EQ @<banfn>
     AND bnfpo    EQ @im_data_new-bnfpo
     AND id_coupa NE @space.

  IF sy-subrc IS INITIAL.
    CLEAR ex_messages.
    messages-type = 'E'.
    messages-id = 'Z01'.
    messages-number = '000'.
    messages-message_v1 = 'Alteração permitida somente '.
    messages-message_v2 = 'pelo COUPA. Item:'.
    messages-message_v3 = im_data_new-bnfpo.
    APPEND messages TO ex_messages.
  ENDIF.

ENDIF.
*** Stefanini - IR240290 - 24/06/2025 - FINC - Fim de Alteração
