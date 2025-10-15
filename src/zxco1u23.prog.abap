*&---------------------------------------------------------------------*
*&  Include           ZXCO1U23
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------*
*                      Controle de Alterações                           *
*-----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                          *
*-----------------------------------------------------------------------*
* 06/08/2025|DEVK9A2NIO|NSEGATIN    |Implem.. Exit Ord. Manut. APP Fiori*
*                                   |Chamado: 186781.                   *
*-----------------------------------------------------------------------*
DATA: BEGIN OF wl_ekpo,
        erekz TYPE ekpo-erekz,
        elikz TYPE ekpo-elikz,
        bnfpo TYPE ekpo-bnfpo,
        banfn TYPE banfn,
      END OF wl_ekpo,



      BEGIN OF wl_eban,
        banfn TYPE eban-banfn,
        ebeln TYPE eban-ebeln,
        frgzu TYPE eban-frgzu,
        bnfpo TYPE eban-bnfpo,
      END OF wl_eban.

DATA: p_respo TYPE c.

DATA: vmsg    TYPE bapi_msg.

**<<<------"186781 - NMS - INI------>>>
*IF ( sy-tcode EQ 'IW32' )
*AND ( sy-ucomm EQ 'LOEA' )."Regra para eliminação de itens na aba componentes.
DATA: status   TYPE char3,
      ck_ordem TYPE char1.

MOVE is_header-sttxt(3) TO status.

* Verifica se a Ordem deve ser Checada.
IF status EQ 'LIB' OR status EQ 'ABE'.
  IF is_header-autyp EQ '30'.
    IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34' OR sy-tcode EQ 'IW21' OR sy-tcode EQ 'IW22'.
      MOVE abap_true TO ck_ordem.

    ENDIF.
* Verifica se a chamada da Exit não está vindo via app Fiori/ Webdynpro
* e se está em processo de criação ou modificação.
    IF   sy-cprog         EQ 'SAPMHTTP'   AND
       ( is_header-create IS NOT INITIAL  OR
         is_header-change IS NOT INITIAL ).
      MOVE abap_true TO ck_ordem.

    ENDIF.

  ENDIF.

ENDIF.

IF ( sy-tcode EQ 'IW31'        OR
     sy-tcode EQ 'IW32'        OR
     sy-tcode EQ 'IW34'        OR
     sy-tcode EQ 'IW21'        OR
     sy-tcode EQ 'IW22'        OR
     ck_ordem IS NOT INITIAL ) AND
   ( sy-ucomm EQ 'LOEA'        OR
     i_fcode  EQ 'LOEA' ).
**<<<------"186781 - NMS - FIM------>>>
* Regra para eliminação de itens na aba componentes.
  SELECT SINGLE banfn ebeln frgzu bnfpo
    FROM eban
    INTO CORRESPONDING FIELDS OF wl_eban
    WHERE banfn = is_component-banfnr
     AND  bnfpo = is_component-banfpo.

  CHECK sy-subrc IS INITIAL.

  IF wl_eban-frgzu = 'X'.
    SELECT SINGLE erekz elikz bnfpo banfn
      FROM ekpo
      INTO CORRESPONDING FIELDS OF wl_ekpo
      WHERE banfn = wl_eban-banfn
       AND  bnfpo = wl_eban-bnfpo.

    IF sy-subrc IS INITIAL.
      CONCATENATE 'Existem requisição aprovada (' wl_eban-banfn '), pedido (' wl_eban-ebeln ') para o componente ('wl_eban-bnfpo').' INTO vmsg.
      MESSAGE vmsg TYPE 'I'.
      MESSAGE e398(00) WITH 'Não foi possível eliminar o item.'.
    ELSE.
      CONCATENATE 'Existem requisição aprovada (' wl_eban-banfn ') para o componente ('wl_eban-bnfpo').' INTO vmsg.
      MESSAGE vmsg TYPE 'I'.
      MESSAGE e398(00) WITH 'Não foi possível eliminar o item.'.
    ENDIF.
  ENDIF.
ENDIF.
*
*DATA: v_frgkz TYPE eban-frgkz VALUE IS INITIAL,
*      v_dismm TYPE marc-dismm VALUE IS INITIAL.
*
**&---------------------------------------------------------------------*
**&  Consistir exclusão de componentes com categoria (N - Req. Compras) *
**&  durante alteração da ordem.                                        *
**&---------------------------------------------------------------------*
*IF ( sy-tcode NE 'IW32' ) AND ( i_fcode EQ 'LOEA' ).
** Verificar se ja houve baixa na reserva
*  IF is_component-enmng GT 0.
*    MESSAGE w003(zpm).
*    RAISE no_changes_allowed.
*  ENDIF.
*
** Se for item não inventáriado.
*  IF is_component-postp EQ 'N'.
**   Se existir requisição de compra
*    IF is_component-banfnr IS NOT INITIAL.
*
**     Recuperar dados da requisição de compras
*      SELECT SINGLE frgkz INTO v_frgkz
*        FROM eban
*        WHERE banfn EQ is_component-banfnr
*          AND bnfpo EQ is_component-banfpo.
*
*      IF v_frgkz EQ 'Z'. "Celson Neves - Chamado 30595 09.04.2012
*        MESSAGE w002(zpm) WITH is_component-banfnr is_component-banfpo.
*        RAISE no_changes_allowed.
*      ENDIF.
*    ENDIF. " Se existir requisição de compra
*  ENDIF. " Se for item não inventáriado
*ENDIF.

***********************************************************************
*  Consitências para Ordens Liberadas do Tipo 30 (PM)                 *
*  nas Transações IW31/IW22/IW34 - Módulo PM                          *
*                                                                     *
***********************************************************************
TYPES: BEGIN OF ty_afvgd.
         INCLUDE STRUCTURE afvgb.
TYPES: END OF ty_afvgd.

TYPES: BEGIN OF ty_resb.
         INCLUDE STRUCTURE resbb.
TYPES: END OF ty_resb.

DATA: it_afvgd      TYPE STANDARD TABLE OF ty_afvgd,
      wa_afvgd      TYPE ty_afvgd,
      it_resb       TYPE STANDARD TABLE OF ty_resb,
      wa_resb       TYPE ty_resb,
      it_resb_atend TYPE STANDARD TABLE OF ty_resb,
      wa_resb_atend TYPE ty_resb. "<<<------"186781 - NMS ------->>>
*      ck_ordem      TYPE char1,  "<<<------"186781 - NMS ------->>>
*      status        TYPE char3.  "<<<------"186781 - NMS ------->>>

FIELD-SYMBOLS: <afvgd> TYPE any,
               <resb>  TYPE any.

MOVE is_header-sttxt(3) TO status.

*
**<<<------"186781 - NMS - INI------>>>
*IF status EQ 'LIB' OR status EQ 'ABE'.
*  IF is_header-autyp EQ '30'.
*    IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34' OR sy-tcode EQ 'IW21' OR sy-tcode EQ 'IW22'.
*      MOVE abap_true TO ck_ordem.
*    ENDIF.
*
*    " Verifica se a chamada da Exit não está vindo via app Fiori/ Webdynpro
*    IF sy-cprog = 'SAPMHTTP'.
*      MOVE abap_true TO ck_ordem.
*    ENDIF.
*
*  ENDIF.
*ENDIF.
**<<<------"186781 - NMS - FIM------>>>
IF ck_ordem EQ abap_true.

  ASSIGN ('(SAPLCOBP)AFVG_BT[]') TO <afvgd>.
  CHECK <afvgd> IS ASSIGNED.
  it_afvgd = <afvgd>.
  ASSIGN ('(SAPLCOBC)RESB_BT[]') TO <resb>.
  IF <resb> IS ASSIGNED.
    it_resb = <resb>.
    DELETE it_resb
      WHERE vornr NE is_component-vornr
      OR vbkz EQ 'X'
      OR vbkz EQ 'D'
      OR xloek EQ abap_true.
    it_resb_atend = <resb>.
    DELETE it_resb_atend
      WHERE vornr NE is_component-vornr
      OR xloek EQ abap_true
      OR ( kzear NE abap_true AND enmng EQ 0 ).
  ENDIF.

  LOOP AT it_afvgd INTO wa_afvgd
    WHERE vornr EQ is_component-vornr.
    IF is_component-enmng GT 0.
      MESSAGE 'Item da Reserva ja possui movimentação, nao é possivel alteração!!' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ENDIF.
    IF wa_afvgd-einsa EQ '1' AND wa_afvgd-ntanf GE sy-datum.                "Data de Op. no Futuro ou no Presente
      "DO NOTHING
    ELSEIF wa_afvgd-einsa EQ '1' AND wa_afvgd-ntanf LT sy-datum             "Data de Op. no passado e sem material
      AND it_resb IS INITIAL.
      MESSAGE 'Data da operação encontra-se no passado, favor atualizar ou utilizar uma nova operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ELSEIF wa_afvgd-einsa EQ '1' AND wa_afvgd-ntanf LT sy-datum             "Data de Op. no passado e sem material atendido
      AND it_resb_atend IS INITIAL.
      MESSAGE 'Data da operação encontra-se no passado, favor atualizar ou utilizar uma nova operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ELSEIF wa_afvgd-einsa EQ '1' AND wa_afvgd-ntanf LT sy-datum             "Data de Op. no passado e com material atendido
      AND it_resb_atend IS NOT INITIAL.
      MESSAGE 'Operação já possui componente atendido. Favor criar nova Operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ELSEIF ( wa_afvgd-einsa NE '1' OR wa_afvgd-ntanf IS INITIAL )           "Data de Op. vazia e sem material
      AND it_resb IS INITIAL.
      MESSAGE 'Favor atualizar data da Operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ELSEIF ( wa_afvgd-einsa NE '1' OR wa_afvgd-ntanf IS INITIAL )           "Data de Op. vazia e com material atendido
      AND it_resb_atend IS NOT INITIAL.
      MESSAGE 'Como a Data não foi preenchida, favor utilizar nova operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ELSEIF ( wa_afvgd-einsa NE '1' OR wa_afvgd-ntanf IS INITIAL )           "Data de Op. vazia e sem material atendido
      AND it_resb_atend IS INITIAL.
      MESSAGE 'Favor atualizar data da Operação' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE no_changes_allowed.
    ENDIF.
  ENDLOOP.

ENDIF.

***********************************************************************
*  Consitências para Ordens Liberadas do Tipo 30 (PM)                 *
*  nas Transações IW31/IW22/IW34 - Módulo PM                          *
*  Materiais Obsoletos                                                *
***********************************************************************
**<<<------"186781 - NMS - INI------>>>
*DATA: v_mmsta      TYPE char2,
*      ck_ordem_obs TYPE char1.
*
*IF status EQ 'LIB' OR status EQ 'ABE'.
*  IF is_header-autyp EQ '30'.
*    IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34' OR sy-tcode EQ 'IW21' OR sy-tcode EQ 'IW22'.
*      MOVE abap_true TO ck_ordem_obs.
*    ENDIF.
*  ENDIF.
*ENDIF.
*
*IF ck_ordem_obs EQ abap_true.

DATA: v_mmsta TYPE char2.
**<<<------"186781 - NMS - FIM------>>>
IF ck_ordem EQ abap_true.

  ASSIGN ('(SAPLCOBP)AFVG_BT[]') TO <afvgd>.
  IF <afvgd> IS ASSIGNED.
    it_afvgd = <afvgd>.
  ENDIF.

  LOOP AT it_afvgd INTO wa_afvgd
    WHERE vornr EQ is_component-vornr.
    IF wa_afvgd-steus NE 'PM03'.                                                                    "Não é Operação de Serviços Externos

      SELECT SINGLE mmsta
        FROM marc
        INTO v_mmsta
        WHERE matnr EQ is_component-matnr
        AND werks EQ wa_afvgd-werks.

      IF sy-subrc EQ 0.
        IF v_mmsta EQ '05'.
          MESSAGE 'Material está marcado como Obsoleto e não pode ser inserido na Operação selecionada' TYPE 'S' DISPLAY LIKE 'E'.
          RAISE no_changes_allowed.
        ENDIF.
      ENDIF.

    ELSE.                                                                                           "É Operação de Serviços Externos

      SELECT SINGLE mmsta
              FROM marc
              INTO v_mmsta
              WHERE matnr EQ is_component-matnr
              AND werks EQ wa_afvgd-werks.

      IF sy-subrc EQ 0.
        IF v_mmsta EQ '05'.
          IF is_component-postp NE 'N'.
            MESSAGE 'Categoria de Item não permitida para material obsoleto' TYPE 'S' DISPLAY LIKE 'E'.
            RAISE no_changes_allowed.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDIF.

***********************************************************************
*  Consitências para Ordens Liberadas do Tipo 30 (PM)                 *
*  nas Transações IW31/IW22/IW34 - Módulo PM                          *
*  Materiais Ociosos                                                  *
***********************************************************************
**<<<------"186781 - NMS - INI------>>>
*DATA: vl_mmsta     TYPE char2,
*      ck_ordem_oci TYPE char1.
*
*IF status EQ 'LIB' OR status EQ 'ABE'.
*  IF is_header-autyp EQ '30'.
*    IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34'.
*      MOVE abap_true TO ck_ordem_oci.
*    ENDIF.
*  ENDIF.
*ENDIF.
**<<<------"186781 - NMS - INI------>>>
*IF ck_ordem_obs EQ abap_true.
DATA: vl_mmsta TYPE char2.

IF ck_ordem EQ abap_true.
**<<<------"186781 - NMS - FIM------>>>
  ASSIGN ('(SAPLCOBP)AFVG_BT[]') TO <afvgd>.
  IF <afvgd> IS ASSIGNED.
    it_afvgd = <afvgd>.
  ENDIF.

  LOOP AT it_afvgd INTO wa_afvgd
    WHERE vornr EQ is_component-vornr.
    IF wa_afvgd-steus NE 'PM03'.                                                                    "Não é Operação de Serviços Externos

      SELECT SINGLE mmsta
        FROM marc
        INTO vl_mmsta
        WHERE matnr EQ is_component-matnr
        AND werks EQ wa_afvgd-werks.

      IF sy-subrc EQ 0.
        IF vl_mmsta EQ '04'.
          IF is_component-postp EQ 'N'.
            MESSAGE 'Material está marcado como Ocioso e não pode ser inserido na Operação selecionada' TYPE 'S' DISPLAY LIKE 'E'.
            RAISE no_changes_allowed.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.                                                                                          "É Operação de Serviços Externos
  ENDLOOP.

ENDIF.

* ---> Chamado MG-5609 - 25.07.2023 - CA
FIELD-SYMBOLS <lv_entries> TYPE entries.
**<<<------"186781 - NMS - INI------>>>
*IF sy-tcode = 'IW32'.
IF ck_ordem IS NOT INITIAL.
**<<<------"186781 - NMS - FIM------>>>
*  ASSIGN ('(SAPLCOMK)RC27X-ENTRIES') TO <lv_entries>.
*  IF <lv_entries> IS ASSIGNED AND
*     <lv_entries> >= 99.
*
*    MESSAGE s018(z01) DISPLAY LIKE 'E'. "Utilizar nova operação, limite de itens p/ mesma operação é de 100 itens
*    RAISE no_changes_allowed.
*  ENDIF.

  IF lines( it_resb ) GT 99.
    MESSAGE s018(z01) DISPLAY LIKE 'E'. "Utilizar nova operação, limite de itens p/ mesma operação é de 100 itens
    RAISE no_changes_allowed.
  ENDIF.

ENDIF.
* <--- Chamado MG-5609 - 25.07.2023 - CA
