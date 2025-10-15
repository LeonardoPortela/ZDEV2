*&---------------------------------------------------------------------*
*&  Include           ZXM02U12
*&---------------------------------------------------------------------*
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IM_REQ_HEADER) TYPE REF TO  IF_PURCHASE_REQUISITION
*"     REFERENCE(IM_T_EBAN) TYPE  MEREQ_T_EBAN
*"     REFERENCE(IM_T_EBKN) TYPE  MEREQ_T_EBKN
*"     REFERENCE(IM_T_EBAN_PERS) TYPE  MEREQ_T_EBAN
*"     REFERENCE(IM_T_EBKN_PERS) TYPE  MEREQ_T_EBKN
*"  EXPORTING
*"     REFERENCE(EX_MESSAGES) TYPE  MEREQ_T_BAPIRET2
*"  EXCEPTIONS
*"      ERROR_MESSAGES
*"----------------------------------------------------------------------

*STATICS: V_COUNT TYPE I.
*
*** Modificação - Eduardo Ruttkowski 0 27/03/2008 >>> INICIO.
  DATA: wl_werks TYPE anlz-werks.
  DATA: it_ebkn2 LIKE ebkn.
  DATA: messages LIKE ex_messages WITH HEADER LINE.
*  *** Modificação - Eduardo Ruttkowski 0 27/03/2008 <<< FIM.


*  ** Modificação - Eduardo Ruttkowski - 06.09.2011 >>> INI
*   Modificação para Bloqueio Requisição de Compras - Investimentos
  DATA: t_t001k   TYPE TABLE OF t001k,
        t_setleaf TYPE TABLE OF setleaf.

  DATA: wl_t001k   TYPE t001k,
        wl_setleaf TYPE setleaf.

  DATA: wl_bukrs TYPE t001k-bukrs,
        wl_posnr TYPE imaka-posnr,
        wl_objnr TYPE imakz-objnr.

  DATA: v_mtart     LIKE mara-mtart,
        it_eban     LIKE eban,
        it_ebkn     LIKE ebkn,
        v_txt(60)   TYPE c,
        s_bukrs     TYPE bukrs,
        v_centro_ag TYPE c LENGTH 1.


  DATA: v_div_ccusto LIKE csks-gsber,
        v_div_centro LIKE t130w-werks,
        v_werks1     LIKE eban-werks,
        v_werks2     LIKE eban-werks,
        v_tabix      LIKE sy-tabix.


  LOOP AT im_t_eban INTO it_eban WHERE loekz = space.

*    IF sy-tcode EQ 'ME52N' OR sy-tcode EQ 'ME53N' OR sy-tcode EQ 'ME54N'.
*     clear:  s_bukrs.
*     SELECT SINGLE vkorg
*     FROM t001w
*     INTO s_bukrs
*     WHERE spras EQ 'P'
*     AND werks EQ it_eban-werks.
*
*      "Verifica se existe empresa do pedido no SET.
*      SELECT SINGLE *
*       FROM setleaf
*       INTO @DATA(i_data)
*         WHERE setname EQ 'MAGGI_EMPRESAS_COUPA'
*           AND valfrom EQ @s_bukrs.
*      IF sy-subrc EQ 0.
*        SELECT SINGLE * FROM eban INTO @DATA(w_eban) WHERE banfn EQ @it_eban-banfn AND bnfpo EQ @it_eban-bnfpo AND status_coupa EQ 'S'.
*        IF sy-subrc EQ 0.
*          CLEAR ex_messages.
*          messages-type = 'E'.
*          messages-id = 'Z01'.
*          messages-number = '000'.
*          messages-message_v1 = 'Requisição criada pelo COUPA, '.
*          messages-message_v2 = 'não é possivel modificar, '.
*          messages-message_v3 = 'item:'.
*          messages-message_v4 = it_eban-bnfpo.
*          APPEND messages TO ex_messages.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    CLEAR: w_eban.

    IF it_eban-knttp = 'K'.   "Quando requisiçao para ativo
      READ TABLE im_t_ebkn WITH KEY bnfpo = it_eban-bnfpo
      INTO it_ebkn2.
      IF sy-subrc = 0.
        " Treinamento LMS
        SELECT SINGLE *
            FROM zmmt0103
            INTO @DATA(wa_zmmt0103)
            WHERE saknr = @it_ebkn2-sakto
            OR    matnr = @it_eban-matnr.
        IF sy-subrc = 0.
          IF wa_zmmt0103-fg_obrig = 'X'.
            IF it_eban-banfn IS INITIAL OR it_eban-banfn+0(1) EQ '#'.
              SELECT  SINGLE *
                 FROM zmmt0105
                 INTO @DATA(wa_zmmt0105)
                 WHERE ebeln   = ' '
                 AND   ebelp   = @it_ebkn2-bnfpo
                 AND   tipo    = 'R'
                 AND   data    = @sy-datum
                 AND   usuario = @sy-uname.
            ELSE.
              SELECT  SINGLE *
                FROM zmmt0105
                INTO @DATA(wa_zmmt0105_1)
                WHERE ebeln   = @it_eban-banfn
                AND   ebelp   = @it_eban-bnfpo
                AND   tipo    = 'R'.
            ENDIF.
            IF sy-subrc NE 0.
*              MESSAGE E000(Z01) WITH 'Este item é obrigatório'
*                                ' informar o treinamento (ABA Dados Cliente).'
*                                'Item: ' IT_EBAN-BNFPO.
              CLEAR ex_messages.
              messages-type = 'E'.
              messages-id = 'Z01'.
              messages-number = '000'.
              messages-message_v1 = 'Este item é obrigatório '.
              messages-message_v2 = 'informar o treinamento (ABA Dados Cliente). '.
              messages-message_v3 = 'Item:'.
              messages-message_v4 = it_eban-bnfpo.
              APPEND messages TO ex_messages.
            ENDIF.
          ELSEIF sy-tcode NE 'ME54N' AND sy-tcode NE ''. "Atenção
            SELECT  SINGLE *
                 FROM zmmt0105
                 INTO @DATA(wa_zmmt0105_2)
                 WHERE ebeln   = ' '
                 AND   ebelp   = @it_eban-bnfpo
                 AND   tipo    = 'R'
                 AND   data    = @sy-datum
                 AND   usuario = @sy-uname.
            IF sy-subrc NE 0.
*              MESSAGE E000(Z01) WITH 'ATENÇÃO responder se é treinamento'
*                                ' informar (Sim/Não) (ABA Dados Cliente).'
*                                'Item: ' IT_EBAN-BNFPO.
              CLEAR ex_messages.
              messages-type = 'E'.
              messages-id = 'Z01'.
              messages-number = '000'.
              messages-message_v1 = 'ATENÇÃO responder se é treinamento'.
              messages-message_v2 = 'informar (Sim/Não) (ABA Dados Cliente).'.
              messages-message_v3 = 'Item:'.
              messages-message_v4 = it_eban-bnfpo.
              APPEND messages TO ex_messages.
            ELSEIF wa_zmmt0105_2-atencao = 'S' AND wa_zmmt0105_2-id_lms IS INITIAL. "respondeu 'S' mas não escolheu
*              MESSAGE E000(Z01) WITH 'ATENÇÃO este item é de treinamento'
*                               ' informar o treinamento (ABA Dados Cliente).'
*                               'Item: ' IT_EBAN-BNFPO.
              CLEAR ex_messages.
              messages-type = 'E'.
              messages-id = 'Z01'.
              messages-number = '000'.
              messages-message_v1 = 'ATENÇÃO responder se é treinamento'.
              messages-message_v2 = 'informar (Sim/Não) (ABA Dados Cliente).'.
              messages-message_v3 = 'Item:'.
              messages-message_v4 = it_eban-bnfpo.
              APPEND messages TO ex_messages.

            ENDIF.

          ENDIF.
        ELSE.
          DELETE FROM zmmt0105
                WHERE ebeln   = it_eban-banfn
                AND   ebelp   = it_eban-bnfpo
                AND   tipo    = 'R'.
        ENDIF.


      ENDIF.
    ENDIF.
  ENDLOOP.
* Se for empresa Amaggi Argentina pula as validações
  SELECT *
    FROM t001k INTO TABLE t_t001k
     FOR ALL ENTRIES IN im_t_eban
   WHERE bwkey = im_t_eban-werks.

  v_centro_ag = ''.

  LOOP AT t_t001k INTO wl_t001k .

    SELECT SINGLE *
      FROM setleaf
      INTO wl_setleaf
     WHERE setname = 'MAGGI_EMPRESA_EXTERIOR'
       AND valfrom = wl_t001k-bukrs.

    IF sy-subrc IS INITIAL .
      v_centro_ag = 'X'.
    ENDIF.

  ENDLOOP.
*************************
  IF v_centro_ag NE 'X'.


    LOOP AT im_t_eban INTO it_eban WHERE knttp = 'F' OR knttp = 'A'.

      LOOP AT im_t_ebkn INTO it_ebkn2 WHERE bnfpo = it_eban-bnfpo.

        CASE it_eban-knttp.

          WHEN 'A'. "Imobilizado
            IF NOT it_ebkn2-anln1 IS INITIAL.
*     Busca empresa
              SELECT SINGLE bukrs FROM t001k INTO wl_bukrs
                WHERE bwkey = it_ebkn2-gsber.

              SELECT SINGLE posnr FROM imaka INTO wl_posnr
                WHERE bukrs = wl_bukrs AND
                      anln1 = it_ebkn2-anln1 AND
                      anln2 = it_ebkn2-anln2.

              IF sy-subrc = 0.
                PERFORM verifica_status TABLES ex_messages
                      USING wl_posnr it_ebkn2-bnfpo.
              ENDIF.
            ENDIF.

          WHEN 'F'.

            CLEAR wl_objnr.
            CONCATENATE 'OR' it_ebkn2-aufnr INTO wl_objnr.

            SELECT SINGLE posnr FROM imakz INTO wl_posnr
              WHERE objnr = wl_objnr.

            IF sy-subrc = 0.
              PERFORM verifica_status TABLES ex_messages USING wl_posnr it_ebkn2-bnfpo.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
    ENDLOOP.
*    ** Modificação - Eduardo Ruttkowski - 06.09.2011 <<< FIM


*    ** Modificação - Eduardo Ruttkowski 0 27/03/2008 >>> INICIO.
    LOOP AT im_t_eban INTO it_eban WHERE loekz = space.
      IF it_eban-knttp = 'A'.   "Quando requisiçao para ativo
        READ TABLE im_t_ebkn WITH KEY bnfpo = it_eban-bnfpo
        INTO it_ebkn2.
        IF sy-subrc = 0.
          SELECT SINGLE bukrs FROM t001k INTO wl_bukrs
               WHERE bwkey = it_ebkn2-gsber.

          SELECT SINGLE werks FROM anlz
            INTO wl_werks
            WHERE bukrs = wl_bukrs AND
                  anln1 = it_ebkn2-anln1 AND
                  anln2 = it_ebkn2-anln2.
          IF sy-subrc = 0.
            IF wl_werks NE it_eban-werks.

              CLEAR ex_messages.
              messages-type = 'E'.
              messages-id = 'Z01'.
              messages-number = '000'.
              messages-message_v1 = 'Centro da Requisição '.
              messages-message_v2 = 'diferente do Centro do '.
              messages-message_v3 = 'Imobilizado - Item:'.
              messages-message_v4 = it_eban-bnfpo.
              APPEND messages TO ex_messages.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    *** Modificação - Eduardo Ruttkowski 0 27/03/2008 <<< FIM.


    DATA: v_count TYPE n.

    IF v_count GT 1.
      v_count = 0.
      EXIT.
    ENDIF.

    v_count = 2.

*    LOOP AT IM_T_EBAN INTO IT_EBAN WHERE LOEKZ = SPACE.
*
*      IF IT_EBAN-MATNR NE SPACE.
*        SELECT SINGLE MTART
*        FROM          MARA
*        INTO          V_MTART
*        WHERE         MATNR = IT_EBAN-MATNR.
*
*        IF SY-SUBRC = 0.
*
*          IF IT_EBAN-KNTTP = 'A'.   "Quando requisiçao para ativo
*
*            IF V_MTART NE 'ZLAG'.
*              CONCATENATE '- Item:'
*                          IT_EBAN-BNFPO INTO V_TXT SEPARATED BY SPACE .
**              CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
**                EXPORTING
**                  TITEL        = 'Tipo do Material x Categoria Classif.Ctbil - Inconsistentes'
**                  TEXTLINE1    = 'Possível inconsistência Tipo do Material x Categoria Classif.Ctbil'
**                  TEXTLINE2    = V_TXT
**                  START_COLUMN = 25
**                  START_ROW    = 6.
*              MESSAGE  E398(Z01) WITH 'Tp.MaterialxCtg Classif.Ctbil - Inconsistentes'
*                        'Contate as Áreas de Suprimentos e Patrimônio'
*                        V_TXT.
*
*            ENDIF.
*
*          ELSE.
*
*            IF V_MTART EQ 'ZLAG'.
*              CONCATENATE '- Item:'
*                          IT_EBAN-BNFPO INTO V_TXT SEPARATED BY SPACE .
*
**              CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
**                EXPORTING
**                  TITEL        = 'Tipo do Material x Categoria Classif.Ctbil - Inconsistentes'
**                  TEXTLINE1    = 'Possível inconsistência Tipo do Material x Categoria Classif.Ctbil'
**                  TEXTLINE2    = V_TXT
**                  START_COLUMN = 25
**                  START_ROW    = 6.
*              MESSAGE  E398(Z01) WITH 'Tp.MaterialxCtg Classif.Ctbil - Inconsistentes'
*                             'Contate as Áreas de Suprimentos e Patrimônio'
*                             V_TXT.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDLOOP.
  ENDIF.
*    ini - cesar coelho - 20.08.2008

  READ TABLE im_t_eban INTO it_eban WITH KEY bsart = 'REG'
                                             infnr = space.
  IF sy-subrc = 0.

    CLEAR ex_messages.
    messages-type = 'E'.
    messages-id = 'Z01'.
    messages-number = '000'.
    messages-message_v1 = 'Obrigat. preenchimento '.
    messages-message_v2 = 'do campo Registro de Info'.
    APPEND messages TO ex_messages.
  ELSE.
    READ TABLE im_t_eban INTO it_eban WITH KEY bsart = 'ZOUT'
                                             infnr = space.
    IF sy-subrc = 0.
      IF it_eban-ekgrp NE 'C40'. "execução
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Obrigat. preenchimento '.
        messages-message_v2 = 'do campo Registro de Info'.
        APPEND messages TO ex_messages.
      ENDIF.
    ELSE.
      READ TABLE im_t_eban INTO it_eban WITH KEY bsart = 'ZMAR'
                                                 infnr = space.

      IF ( sy-subrc EQ 0 ).
        CLEAR ex_messages.
        messages-type = 'E'.
        messages-id = 'Z01'.
        messages-number = '000'.
        messages-message_v1 = 'Obrigat. preenchimento '.
        messages-message_v2 = 'do campo Registro de Info'.
        APPEND messages TO ex_messages.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA(_erro) = ''.
  "Bloqueio COUPA********
  IF ( sy-ucomm = 'MECHECKDOC'  OR sy-ucomm = 'MESAVE'  ).
    READ TABLE im_t_eban INTO it_eban INDEX 1.
    v_werks1  = it_eban-werks.
    SELECT SINGLE *
      FROM zmmt0156
      INTO @DATA(w56)
      WHERE werks = @v_werks1
      AND   tcode = @sy-tcode.
    IF sy-subrc = 0 OR
       sy-tcode = 'ME53N'.
      SELECT SINGLE *
         FROM zmmt0157
         INTO @DATA(w57)
         WHERE werks = @v_werks1
         AND   tcode = @sy-tcode
         AND   usnam = @sy-uname.
      IF sy-subrc NE 0.
        IF sy-tcode = 'ME53N'.
          CLEAR ex_messages.
          messages-type = 'E'.
          messages-id = 'Z01'.
          messages-number = '000'.
          messages-message_v1 = 'Transação ME53N não autorizada para alterar,' .
          messages-message_v2 = 'Utilize a ME51N/ME52N'.
          APPEND messages TO ex_messages.
        ELSE.
          CLEAR ex_messages.
          messages-type = 'E'.
          messages-id = 'Z01'.
          messages-number = '000'.
          messages-message_v1 = |Transação { sy-tcode } | .
          messages-message_v2 = 'não autorizada, procure depto de Suprimentos'.
          messages-message_v3 = 'corporativo'.
          APPEND messages TO ex_messages.
        ENDIF.
        _erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  "Bloqueio COUPA********

*---------------------------------------------------------------------
*  CS2020001133 Criar regra para aviso de estoque para RC´s para
*Centro de custo e Ordem
*---------------------------------------------------------------------
  IF ( ( sy-tcode EQ 'ME51N' OR sy-tcode EQ 'ME52N' OR sy-tcode EQ 'ME53N' )
      AND ( sy-ucomm = 'MECHECKDOC' ) OR sy-ucomm = 'MESAVE' )
      AND _erro IS INITIAL.

    TYPES: BEGIN OF ty_mard,
             material  TYPE mard-matnr,
             descricao TYPE makt-maktx,
             centro    TYPE mard-werks,
             deposito  TYPE mard-lgort,
             saldo     TYPE mard-labst,
           END OF ty_mard.

    DATA: t_itens TYPE TABLE OF ty_mard,
          w_itens LIKE LINE OF t_itens.

    DATA(it_eban_aux) = im_t_eban.
*
*    "Desconsidera itens sem informações para seleção
*    DELETE it_eban_aux WHERE ( ( knttp <> 'K' AND knttp <> 'F' )
*                               OR matnr IS INITIAL ).

    DELETE it_eban_aux WHERE matnr IS INITIAL.

    "Buscar Saldo disponível nos depósitos
    IF it_eban_aux[] IS NOT INITIAL.

      SELECT matnr, werks, lgort, labst FROM mard
        INTO TABLE @DATA(it_mard)
        FOR ALL ENTRIES IN @it_eban_aux
            WHERE matnr = @it_eban_aux-matnr.

      "Elimina registros sem saldo
      DELETE it_mard WHERE labst IS INITIAL.

      IF it_mard[] IS NOT INITIAL.

        "Descrição do Material
        SELECT matnr, maktx FROM makt
          INTO TABLE @DATA(t_makt)
          FOR ALL ENTRIES IN @it_mard
          WHERE matnr = @it_mard-matnr
          AND   spras = @sy-langu(1).

      ENDIF.

      "Monta tabela de Saída
      LOOP AT it_mard INTO DATA(w_mard).

        PACK w_mard-matnr TO w_itens-material.
        CONDENSE w_itens-material.

        w_itens-centro   = w_mard-werks.
        w_itens-deposito = w_mard-lgort.
        w_itens-saldo    = w_mard-labst.

        "Preenche descrição da empresa
        READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_mard-matnr.
        IF sy-subrc IS INITIAL.
          w_itens-descricao = w_makt-maktx.
        ENDIF.

        APPEND w_itens TO t_itens.

      ENDLOOP.

      IF t_itens[] IS NOT INITIAL.

        cl_demo_output=>new(
          )->begin_section( 'Saldo disponível nos depósitos'
          )->end_section(
          )->display( t_itens ).

      ENDIF.

    ENDIF.

  ENDIF.
