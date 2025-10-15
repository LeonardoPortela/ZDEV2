"Name: \PR:SAPLMEREQ\TY:LCL_REQ_ITEM\ME:PREPARE_POST\SE:END\EI
ENHANCEMENT 0 ZLMEREQF05.
*** ROLLOUT *** Eduardo Ruttkowski Tavares - 02.09.2009
**estratégia de liberação RCC – Req.Corporativa
if sy-tcode = 'ME51N'
or sy-tcode = 'ME52N'
or sy-tcode = 'ME53N' "  CS2016001503
or SY-TCODE = 'ZMM0072'
or sy-cprog = 'ZMMR047'.
*EX_ueban

data: w_esll type esll,
      t_zmm_aprov_rcc  type zmm_aprov_rcc.

*DATA:
*  LD_ANSWER    TYPE STRING,
*  IT_T_SPOPLI  TYPE STANDARD TABLE OF SPOPLI, "TABLES PARAM
*  WA_T_SPOPLI  LIKE LINE OF IT_T_SPOPLI.
*
*
*DATA(LD_CURSORLINE) = 123 .
*DATA(LD_MARK_FLAG) = 'some text here'.
*DATA(LD_MARK_MAX) = 'some text here'.
*DATA(LD_START_COL) = 10 .
*DATA(LD_START_ROW) = 5 .
*DATA(LD_TEXTLINE1) = ''.
*DATA(LD_TEXTLINE2) = ''.
*DATA(LD_TEXTLINE3) = ''.
*DATA(LD_TITEL) = 'Destinação'.
*DATA(LD_DISPLAY_ONLY) = ''.
*
*DATA W_ANSWER(1).
*DATA W_ANSWER2(1).
*
*IMPORT W_ANSWER TO W_ANSWER FROM MEMORY ID 'MDESTI'.
*
*    if EX_ueban-PSTYP ne '9'.
*      select SINGLE *
*        from mbew
*        into @DATA(W_MBEW)
*        where MATNR = @EX_ueban-matnr
*        and   BWKEY = @EX_ueban-werks.
*      IF W_MBEW-MTUSE = '0' or W_MBEW-MTUSE = '1'.
*         if W_ANSWER NE '1' and W_ANSWER is not INITIAL.
*             MESSAGE e000(z01) WITH 'Todas as Req. de compra devem ter a mesma destinação!' EX_ueban-matnr.
*         else.
*            W_ANSWER = '1'. "produção
*         endif.
*      ENDIF.
*    endif.
*
*    if W_ANSWER IS INITIAL.
*      "populate fields of struture and append to itab
*        refresh IT_T_SPOPLI.
*        WA_T_SPOPLI-VAROPTION = '1-Produtivo'.
*        APPEND WA_T_SPOPLI TO IT_T_SPOPLI.
*
*        WA_T_SPOPLI-VAROPTION = '2-Administrativo'.
*        APPEND WA_T_SPOPLI TO IT_T_SPOPLI.
*
*        WA_T_SPOPLI-VAROPTION = 'Ajuda'.
*        APPEND WA_T_SPOPLI TO IT_T_SPOPLI.
*        WHILE 1 = 1.
*          CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
*             EXPORTING
**              cursorline         = ld_cursorline
**              mark_flag          = ld_mark_flag
**              mark_max           = ld_mark_max
*               START_COL          = LD_START_COL
*               START_ROW          = LD_START_ROW
*               TEXTLINE1          = LD_TEXTLINE1
**              textline2          = ld_textline2
**              textline3          = ld_textline3
*               TITEL              = LD_TITEL
**              display_only       = ld_display_only
*             IMPORTING
*               ANSWER             = LD_ANSWER
*             TABLES
*               T_SPOPLI           = IT_T_SPOPLI
*             EXCEPTIONS
*               NOT_ENOUGH_ANSWERS = 1
*               TOO_MUCH_ANSWERS   = 2
*               TOO_MUCH_MARKS     = 3.
*
*               IF LD_ANSWER = 3.
*                   DATA(TEXTO_HTML) =
*                       '<!DOCTYPE html>' &&
*                       '<html>' &&
*                       '<body>' &&
*                       '<h1>Help Destinação</h1>' &&
*                       '<p>PRODUTIVO: Deve ser selecionada esta destinação na aquisição de insumos, ' &&
*                       'maquinários agrícolas e peças e serviços para manutenção de ativo imobilizado ' &&
*                       'ligados à área produtiva. </p>' &&
*                       '<p>Exemplo: compra de uma colheitadeira, compra de uma peça e aquisição de serviços' &&
*                       'para manutenção de colheitadeira, combustível para abastecimento de máquinas e ' &&
*                       'equipamentos ligados à produção.</p>' &&
*                       '<p>ADMINISTRATIVO: Deve ser selecionada esta destinação na aquisição de materiais ' &&
*                       'que serão utilizados apenas no administrativo, ou seja, que não estão ligados à ' &&
*                       'produção. </p>' &&
*                       '<p>Exemplo: compra materiais de escritório (lápis, caneta, papel), combustível de ' &&
*                       'veículos não ligados à produção, alimentos.</p>' &&
*                       '</body>' &&
*                       '</html>'.
*
*                      CL_ABAP_BROWSER=>SHOW_HTML(
*                        EXPORTING
*                          HTML_STRING  = TEXTO_HTML
*                          TITLE        =  'Ajuda para Destinação'
*                           MODAL        = ABAP_TRUE
*                      ).
*               ELSE.
*                  exit.
*               ENDIF.
*        Endwhile.
*
*        EX_ueban-ZDESTI = LD_ANSWER.
*        EXPORT W_ANSWER FROM W_ANSWER TO MEMORY ID 'MDESTI'.
*    else.
*        EX_ueban-ZDESTI = W_ANSWER.
*    endif.


    IF EX_ueban-bsart = 'RCC' AND
       EX_ueban-pstyp = 9.
        FIELD-SYMBOLS: <f_esll> TYPE TABLE.
        CONSTANTS: cc_esll(28) VALUE '(SAPLMLSP)IX_ESLL[]'.
        ASSIGN (cc_esll) TO <f_esll>.

        IF <f_esll> IS ASSIGNED.
          DATA: Tw_IX_ESLL TYPE STANDARD TABLE OF ZIX_ESLL,
                W_IX_ESLL TYPE ZIX_ESLL.

          tw_ix_esll[] = <f_esll>.
            sort tw_ix_esll by MSUPDAP-SRVPOS DESCENDING.
*            READ TABLE tw_ix_esll  INTO  W_IX_ESLL index 1.
            LOOP at tw_ix_esll  INTO  W_IX_ESLL.
                IF W_IX_ESLL-MSUPDAP-SRVPOS is INITIAL.
                    CONTINUE.
                ENDIF.

                SELECT SINGLE *
                  FROM zmm_aprov_rcc
                  INTO t_zmm_aprov_rcc
                  WHERE srvpos EQ W_IX_ESLL-MSUPDAP-SRVPOS.

                IF sy-subrc EQ 0.
                  MOVE t_zmm_aprov_rcc-frgst TO EX_ueban-frgst.
                ELSE.
                  MESSAGE e000(z01) WITH 'Serviço/material não permitido para'
                                         'requisição corporativa'
                                         W_IX_ESLL-MSUPDAP-SRVPOS.
                ENDIF.
              ENDLOOP.
        ENDIF.
    ELSEIF EX_ueban-bsart = 'RCC' AND
           EX_ueban-matnr <> 0.

      SELECT SINGLE *
        FROM zmm_aprov_rcc
        INTO t_zmm_aprov_rcc
        WHERE matnr EQ EX_ueban-matnr.

      IF sy-subrc EQ 0.
        MOVE t_zmm_aprov_rcc-frgst TO EX_ueban-frgst.
      ELSE.
        MESSAGE e000(z01) WITH 'Serviço informado não cadastrado para este tipo de requisição '
                               'procurar Área de Suprimento.'.
      ENDIF.
    ENDIF.

endif.
ENDENHANCEMENT.
