*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_P8000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8000 INPUT.

  CASE ok_code.
    WHEN c_prpesq.
      CLEAR ok_code.
      PERFORM pesquisar_protocolos USING space.
    WHEN c_lancme.
      CLEAR ok_code.
      PERFORM consulta_protocolo.
    WHEN c_lancnv.
      CLEAR ok_code.
      PERFORM lancar_protocolo.
    WHEN c_edmemo.
      CLEAR ok_code.
      PERFORM editar_protocolo.
    WHEN c_dlproto.
      CLEAR ok_code.
      PERFORM eliminar_protocolo.
    WHEN c_vcmemo.
      CLEAR ok_code.
      PERFORM vincular_memorandos.
    WHEN c_csprint.
      CLEAR ok_code.
      PERFORM imprimir_protocolo.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_8000  INPUT

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_PROTOCOLO
*&---------------------------------------------------------------------*
*       Imprimir protocolo
*----------------------------------------------------------------------*
FORM imprimir_protocolo .

  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_protocolo USING vg_verifica_selecao.

  IF vg_verifica_selecao IS INITIAL.
    PERFORM emitir_protoloco USING wa_protocolos.
  ENDIF.

ENDFORM.                    " IMPRIMIR_PROTOCOLO


*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_PROTOCOLOS
*&---------------------------------------------------------------------*
*       Pesquisa de Protocolos de Envio de Memorando
*----------------------------------------------------------------------*
FORM pesquisar_protocolos USING pesq TYPE c.

  TYPES: BEGIN OF ty_forne.
  TYPES: lifnr TYPE lifnr,
         name1 TYPE name1_gp.
  TYPES: END OF ty_forne.

  TYPES: BEGIN OF ty_empresa.
  TYPES: bukrs TYPE	bukrs,
         butxt TYPE butxt.
  TYPES: END OF ty_empresa.

  TYPES: BEGIN OF ty_filial.
  TYPES: bukrs  TYPE bukrs,
         branch	TYPE j_1bbranc_,
         name	  TYPE name1.
  TYPES: END OF ty_filial.

  DATA: it_memo_vinc  TYPE TABLE OF zdoc_memo_pro_me INITIAL SIZE 0 WITH HEADER LINE,
        it_prot       TYPE TABLE OF zmemo_protocolo INITIAL SIZE 0 WITH HEADER LINE,
        vg_tabix      TYPE sy-tabix,
        it_empe       TYPE TABLE OF ty_empresa,
        it_empd       TYPE TABLE OF ty_empresa,
        it_filiale    TYPE TABLE OF ty_filial,
        it_filiald    TYPE TABLE OF ty_filial,
        it_emissores  TYPE TABLE OF ty_forne,
        it_destinos   TYPE TABLE OF ty_forne,
        wa_fornecedor TYPE ty_forne,
        wa_empresa    TYPE ty_empresa,
        wa_filial     TYPE ty_filial.

  IF pesq IS INITIAL.

    CLEAR: it_protocolos[].
    IF t_memop IS INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_prot
        FROM zdoc_memo_protoc
       WHERE cd_emissor   IN t_emissp
         AND cd_recebedor IN t_destin
         AND dt_recibo    IN t_recibo.
    ELSE.
      SELECT pt~nr_protocolo pt~emissor   pt~cd_emissor pt~cd_recebedor
             pt~empresa_em   pt~filial_em pt~empresa_de pt~filial_de
             pt~dt_protocolo pt~dt_recibo
        INTO CORRESPONDING FIELDS OF TABLE it_prot
        FROM zdoc_memo_protoc AS pt
       WHERE EXISTS ( SELECT * FROM zdoc_memo_pro_me AS me
                       WHERE me~nr_protocolo EQ pt~nr_protocolo
                         AND me~nr_memorando EQ t_memop ).
    ENDIF.

    IF NOT t_emisnf IS INITIAL.

      SELECT mv~nr_protocolo mv~nr_memorando
        INTO CORRESPONDING FIELDS OF TABLE it_memo_vinc
        FROM zdoc_memo_pro_me AS mv
        INNER JOIN zdoc_memorando   AS me ON me~nr_memorando EQ mv~nr_memorando
        INNER JOIN zdoc_memo_nf_exp AS nf ON nf~nr_nota_exp  EQ me~nr_nota_exp
         FOR ALL ENTRIES IN it_protocolos
       WHERE mv~nr_protocolo EQ it_protocolos-nr_protocolo
         AND nf~emissor IN t_emisnf.

      SORT it_memo_vinc BY nr_protocolo.
      DELETE ADJACENT DUPLICATES FROM it_memo_vinc COMPARING nr_protocolo.

      LOOP AT it_prot INTO wa_protocolos.
        READ TABLE it_memo_vinc WITH KEY nr_protocolo = wa_protocolos-nr_protocolo.
        IF sy-subrc IS INITIAL.
          APPEND wa_protocolos TO it_protocolos[].
        ENDIF.
      ENDLOOP.

    ELSE.
      MOVE it_prot[] TO it_protocolos[].
    ENDIF.
  ENDIF.

  "Emissores
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY cd_emissor.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING cd_emissor.
  SELECT lifnr name1
    INTO CORRESPONDING FIELDS OF TABLE it_emissores
    FROM lfa1
    FOR ALL ENTRIES IN it_prot
   WHERE lifnr EQ it_prot-cd_emissor.
  SORT it_emissores BY lifnr.

  "Destinatario
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY cd_recebedor.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING cd_recebedor.
  SELECT lifnr name1
    INTO CORRESPONDING FIELDS OF TABLE it_destinos
    FROM lfa1
    FOR ALL ENTRIES IN it_prot
   WHERE lifnr EQ it_prot-cd_recebedor.
  SORT it_destinos BY lifnr.

  "Empresa Emissão
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY empresa_em.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING empresa_em.
  SELECT bukrs butxt
    INTO CORRESPONDING FIELDS OF TABLE it_empe
    FROM t001
     FOR ALL ENTRIES IN it_prot
   WHERE bukrs EQ it_prot-empresa_em.
  SORT it_empe BY bukrs.

  "Empresa Destino
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY empresa_de.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING empresa_de.
  SELECT bukrs butxt
    INTO CORRESPONDING FIELDS OF TABLE it_empd
    FROM t001
     FOR ALL ENTRIES IN it_prot
   WHERE bukrs EQ it_prot-empresa_de.
  SORT it_empd BY bukrs.

  "Filial Emissão
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY empresa_em filial_em.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING empresa_em filial_em.
  SELECT bukrs branch name
    INTO CORRESPONDING FIELDS OF TABLE it_filiale
    FROM j_1bbranch
     FOR ALL ENTRIES IN it_prot
   WHERE bukrs  EQ it_prot-empresa_em
     AND branch EQ it_prot-filial_em.
  SORT it_filiale BY bukrs branch.

  "Filial Destino
  MOVE it_protocolos[] TO it_prot[].
  SORT it_prot BY empresa_de filial_de.
  DELETE ADJACENT DUPLICATES FROM it_prot COMPARING empresa_de filial_de.
  SELECT bukrs branch name
    INTO CORRESPONDING FIELDS OF TABLE it_filiald
    FROM j_1bbranch
     FOR ALL ENTRIES IN it_prot
   WHERE bukrs  EQ it_prot-empresa_de
     AND branch EQ it_prot-filial_de.
  SORT it_filiald BY bukrs branch.

  LOOP AT it_protocolos INTO wa_protocolos.
    vg_tabix = sy-tabix.

    READ TABLE it_emissores INTO wa_fornecedor WITH KEY lifnr = wa_protocolos-cd_emissor.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_emissor = wa_fornecedor-name1. ENDIF.

    READ TABLE it_destinos INTO wa_fornecedor WITH KEY lifnr = wa_protocolos-cd_recebedor.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_destino = wa_fornecedor-name1. ENDIF.

    READ TABLE it_empe INTO wa_empresa WITH KEY bukrs = wa_protocolos-empresa_em.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_empresa_em = wa_empresa-butxt. ENDIF.

    READ TABLE it_empd INTO wa_empresa WITH KEY bukrs = wa_protocolos-empresa_de.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_empresa_de = wa_empresa-butxt. ENDIF.

    READ TABLE it_filiale INTO wa_filial WITH KEY bukrs = wa_protocolos-empresa_em branch = wa_protocolos-filial_em.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_filial_em = wa_filial-name. ENDIF.

    READ TABLE it_filiald INTO wa_filial WITH KEY bukrs = wa_protocolos-empresa_de branch = wa_protocolos-filial_de.
    IF sy-subrc IS INITIAL. wa_protocolos-nm_filial_de = wa_filial-name. ENDIF.

    MODIFY it_protocolos INDEX vg_tabix FROM wa_protocolos
    TRANSPORTING nm_emissor nm_destino nm_empresa_em nm_filial_em nm_empresa_de nm_filial_de.
  ENDLOOP.

  SORT it_protocolos[] BY dt_protocolo nr_protocolo.

ENDFORM.                    " PESQUISAR_PROTOCOLOS

*&---------------------------------------------------------------------*
*&      Module  STATUS_8022  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8022 OUTPUT.

  IF zdoc_memo_protoc-dt_protocolo IS INITIAL.
    zdoc_memo_protoc-dt_protocolo = sy-datum.
  ENDIF.

  "Emissores
  IF NOT zdoc_memo_protoc-cd_emissor IS INITIAL.
    SELECT SINGLE name1 INTO vg_nm_emissor
      FROM lfa1
     WHERE lifnr EQ zdoc_memo_protoc-cd_emissor.
  ENDIF.

  "Destinatario
  IF NOT zdoc_memo_protoc-cd_recebedor IS INITIAL.
    SELECT SINGLE name1 INTO vg_nm_destino
      FROM lfa1
     WHERE lifnr EQ zdoc_memo_protoc-cd_recebedor.
  ENDIF.

  "Empresa Emissão
  IF NOT zdoc_memo_protoc-empresa_em IS INITIAL.
    SELECT SINGLE butxt INTO vg_nm_empresa_em
      FROM t001
     WHERE bukrs EQ zdoc_memo_protoc-empresa_em.

    "Filial Emissão
    IF NOT zdoc_memo_protoc-filial_em IS INITIAL.
      SELECT SINGLE name INTO vg_nm_filial_em
        FROM j_1bbranch
       WHERE bukrs  EQ zdoc_memo_protoc-empresa_em
         AND branch EQ zdoc_memo_protoc-filial_em.
    ENDIF.
  ENDIF.

  "Empresa Destino
  IF NOT zdoc_memo_protoc-empresa_de IS INITIAL.
    SELECT SINGLE butxt INTO vg_nm_empresa_de
      FROM t001
     WHERE bukrs EQ zdoc_memo_protoc-empresa_de.

    "Filial Destino
    IF NOT zdoc_memo_protoc-filial_de IS INITIAL.
      SELECT SINGLE name INTO vg_nm_filial_de
        FROM j_1bbranch
       WHERE bukrs  EQ zdoc_memo_protoc-empresa_de
         AND branch EQ zdoc_memo_protoc-filial_de.
    ENDIF.
  ENDIF.

  PERFORM trava_tela_protocolo.

ENDMODULE.                 " STATUS_8022  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_selecao_protocolo  USING  p_vg_verifica_selecao TYPE sy-subrc.

  READ TABLE it_protocolos INTO wa_protocolos WITH KEY mark = c_x.
  p_vg_verifica_selecao = sy-subrc.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE text-e10 TYPE c_e DISPLAY LIKE c_s.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  LANCAR_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lancar_protocolo .

  vg_dynnr_ant = vg_dynnr_000.
  vg_dynnr_000 = c_8020.
  CLEAR vg_consul_prot.
  CLEAR: zdoc_memo_protoc.

ENDFORM.                    " LANCAR_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  EDITAR_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM editar_protocolo .

  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_protocolo USING vg_verifica_selecao.

  IF vg_verifica_selecao IS INITIAL.
    vg_dynnr_ant   = vg_dynnr_000.
    vg_dynnr_000   = c_8020.
    CLEAR vg_consul_prot.
    MOVE-CORRESPONDING wa_protocolos TO zdoc_memo_protoc.
  ENDIF.

ENDFORM.                    " EDITAR_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_protocolo .

  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_protocolo USING vg_verifica_selecao.

  IF vg_verifica_selecao IS INITIAL.
    vg_dynnr_ant   = vg_dynnr_000.
    vg_dynnr_000   = c_8020.
    vg_consul_prot = c_x.
    MOVE-CORRESPONDING wa_protocolos TO zdoc_memo_protoc.
  ENDIF.

ENDFORM.                    " CONSULTA_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8020 INPUT.

  CASE ok_code.
    WHEN c_save.
      CLEAR: ok_code.
      PERFORM salvar_protocolo.
    WHEN c_backv OR c_exitv OR c_cancelv.
      CLEAR: ok_code.
      PERFORM sair_protocolo.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_8020  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_PROTOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_protoc INPUT.
  vg_alterou_protocolo = c_x.
ENDMODULE.                 " SET_UPDATE_PROTOC  INPUT

*&---------------------------------------------------------------------*
*&      Form  SAIR_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sair_protocolo .

  DATA: answer TYPE c LENGTH 1.

  IF NOT vg_alterou_protocolo IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = text-001
        textline1 = text-002
        textline2 = text-003
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN c_j.
        PERFORM salvar_protocolo.
      WHEN c_n.
        CLEAR vg_alterou_protocolo.
        vg_dynnr_000 = vg_dynnr_ant.
        CLEAR: zdoc_memo_protoc.
      WHEN c_a.
        EXIT.
    ENDCASE.

  ELSE.
    vg_dynnr_000 = vg_dynnr_ant.
    CLEAR: zdoc_memo_protoc.
  ENDIF.

ENDFORM.                    " SAIR_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  ALTERAR_STATUS_PROTOCOLO
*&---------------------------------------------------------------------*
*       Atualizar Status de Memorandos
*----------------------------------------------------------------------*
FORM alterar_status_protocolo  USING  vg_protocolos TYPE zmemo_protocolo.

  DATA: it_memo_prot TYPE TABLE OF zdoc_memo_pro_me INITIAL SIZE 0 WITH HEADER LINE,
        wa_memo_prot TYPE zdoc_memo_pro_me,
        wa_doc_memo  TYPE zdoc_memorando.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_memo_prot
    FROM zdoc_memo_pro_me
   WHERE nr_protocolo EQ zdoc_memo_protoc-nr_protocolo.

  LOOP AT it_memo_prot INTO wa_memo_prot.

    SELECT SINGLE * INTO wa_doc_memo
      FROM zdoc_memorando
     WHERE nr_memorando EQ wa_memo_prot-nr_memorando.

    IF sy-subrc IS INITIAL.
      wa_doc_memo-status = c_f.
      MODIFY zdoc_memorando FROM wa_doc_memo.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ALTERAR_STATUS_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Module  VALIDA_DT_RECEBIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_dt_recebimento INPUT.

  DATA: it_memo_prot TYPE TABLE OF zdoc_memo_pro_me INITIAL SIZE 0 WITH HEADER LINE.

  IF ( zdoc_memo_protoc-dt_recibo IS NOT INITIAL ).
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_memo_prot
      FROM zdoc_memo_pro_me
     WHERE nr_protocolo EQ zdoc_memo_protoc-nr_protocolo.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE e057 DISPLAY LIKE c_s.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDA_DT_RECEBIMENTO  INPUT


*&---------------------------------------------------------------------*
*&      Form  SALVAR_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar_protocolo .

  DATA: vg_insert TYPE c LENGTH 1,
        wa_protoc TYPE zmemo_protocolo.

  CHECK NOT vg_alterou_protocolo IS INITIAL.

  "Verificar campos.
  IF zdoc_memo_protoc-dt_protocolo IS INITIAL. MESSAGE e056 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-cd_emissor   IS INITIAL. MESSAGE e050 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-empresa_em   IS INITIAL. MESSAGE e051 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-filial_em    IS INITIAL. MESSAGE e052 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-cd_recebedor IS INITIAL. MESSAGE e053 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-empresa_de   IS INITIAL. MESSAGE e054 DISPLAY LIKE c_s.   ENDIF.
  IF zdoc_memo_protoc-filial_de    IS INITIAL. MESSAGE e055 DISPLAY LIKE c_s.   ENDIF.

  IF zdoc_memo_protoc-nr_protocolo IS INITIAL.
    vg_insert = c_x.
    SELECT MAX( nr_protocolo )
      INTO zdoc_memo_protoc-nr_protocolo
      FROM zdoc_memo_protoc.
    IF zdoc_memo_protoc-nr_protocolo IS INITIAL.
      zdoc_memo_protoc-nr_protocolo = 1.
    ELSE.
      zdoc_memo_protoc-nr_protocolo = zdoc_memo_protoc-nr_protocolo + 1.
    ENDIF.
    zdoc_memo_protoc-emissor = sy-uname.
  ENDIF.

  MODIFY zdoc_memo_protoc FROM zdoc_memo_protoc.

  CLEAR: vg_alterou_protocolo.

  MOVE-CORRESPONDING zdoc_memo_protoc TO wa_protocolos.

  IF wa_protocolos-dt_recibo IS NOT INITIAL.
    PERFORM alterar_status_protocolo USING wa_protocolos.
  ENDIF.

  COMMIT WORK.

  IF NOT vg_insert IS INITIAL.
    APPEND wa_protocolos TO it_protocolos.
    PERFORM pesquisar_protocolos USING c_x.
  ELSE.
    LOOP AT it_protocolos INTO wa_protoc WHERE nr_protocolo EQ wa_protocolos-nr_protocolo.
      MOVE-CORRESPONDING wa_protocolos TO wa_protoc.
      MODIFY it_protocolos INDEX sy-tabix FROM wa_protoc.
    ENDLOOP.
    PERFORM pesquisar_protocolos USING c_x.
  ENDIF.

ENDFORM.                    " SALVAR_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  TRAVA_TELA_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trava_tela_protocolo .
  IF NOT zdoc_memo_protoc-nr_protocolo IS INITIAL.
    LOOP AT SCREEN.
      IF ( screen-name(16) EQ 'ZDOC_MEMO_PROTOC' ) AND ( screen-name+17(9) NE 'DT_RECIBO' ).
        screen-input  = c_0.
        screen-output = c_1.
        MODIFY SCREEN.
      ENDIF.
      IF ( screen-name+17(9) EQ 'DT_RECIBO' ).
        IF vg_consul_prot IS INITIAL.
          screen-input  = c_1.
          screen-output = c_1.
        ELSE.
          screen-input  = c_0.
          screen-output = c_1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF zdoc_memo_protoc-nr_protocolo IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name+17(9) EQ 'DT_RECIBO'.
        screen-input  = c_0.
        screen-output = c_1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " TRAVA_TELA_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM eliminar_protocolo .

  DATA: vg_verifica_selecao TYPE sy-subrc,
        answer TYPE c LENGTH 1.

  PERFORM verifica_selecao_protocolo USING vg_verifica_selecao.

  PERFORM valida_memo_vinculados USING wa_protocolos-nr_protocolo.

  IF vg_verifica_selecao EQ 0.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = text-001
        textline1 = text-006
        textline2 = text-005
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN c_j.
        CLEAR vg_verifica_selecao.
      WHEN c_n.
        vg_verifica_selecao = 4.
      WHEN c_a.
        vg_verifica_selecao = 4.
    ENDCASE.

    IF vg_verifica_selecao IS INITIAL.
      DELETE FROM zdoc_memo_protoc  WHERE nr_protocolo EQ wa_protocolos-nr_protocolo.
      COMMIT WORK.
      DELETE it_protocolos WHERE nr_protocolo EQ wa_protocolos-nr_protocolo.
      CLEAR: wa_protocolos.
    ENDIF.

  ENDIF.

ENDFORM.                    " ELIMINAR_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Module  STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8000 OUTPUT.
  IF NOT t_memop IS INITIAL.
    PERFORM pesquisar_protocolos USING space.
  ENDIF.
ENDMODULE.                 " STATUS_8000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_MEMORANDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vincular_memorandos .

  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_protocolo USING vg_verifica_selecao.

  IF vg_verifica_selecao IS INITIAL.
    vg_dynnr_ant   = vg_dynnr_000.
    vg_dynnr_000   = c_8050.
    MOVE-CORRESPONDING wa_protocolos TO zdoc_memo_protoc.
    PERFORM popula_memorandos_vinculacao USING zdoc_memo_protoc-nr_protocolo.
    PERFORM popula_memorandos_vinculados USING zdoc_memo_protoc-nr_protocolo.
  ENDIF.

ENDFORM.                    " VINCULAR_MEMORANDOS

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8050 INPUT.

  CASE ok_code.
    WHEN c_save.
      CLEAR: ok_code.
      PERFORM salvar_protocolo_vinculacao.
    WHEN c_backv OR c_exitv OR c_cancelv.
      CLEAR: ok_code.
      PERFORM sair_protocolo_vinculacao.
    WHEN c_btmmv.
      CLEAR: ok_code.
      PERFORM vincular_memorando_protocolo.
    WHEN c_btmmd.
      CLEAR: ok_code.
      PERFORM desvincula_memorando_protocolo.
    WHEN c_btcan.
      CLEAR: ok_code.
      PERFORM cancela_memorando_protocolo.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_8050  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_8051  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8051 OUTPUT.

  "Emissores
  IF NOT zdoc_memo_protoc-cd_emissor IS INITIAL.
    SELECT SINGLE name1 INTO vg_nm_emissor
      FROM lfa1
     WHERE lifnr EQ zdoc_memo_protoc-cd_emissor.
  ENDIF.

  "Destinatario
  IF NOT zdoc_memo_protoc-cd_recebedor IS INITIAL.
    SELECT SINGLE name1 INTO vg_nm_destino
      FROM lfa1
     WHERE lifnr EQ zdoc_memo_protoc-cd_recebedor.
  ENDIF.

ENDMODULE.                 " STATUS_8051  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SAIR_PROTOCOLO_VINCULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sair_protocolo_vinculacao .

  DATA: answer TYPE c LENGTH 1.

  IF NOT vg_alterou_protocolo IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = text-001
        textline1 = text-002
        textline2 = text-003
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN c_j.
        PERFORM salvar_protocolo_vinculacao.
      WHEN c_n.
        CLEAR vg_alterou_protocolo.
        vg_dynnr_000 = vg_dynnr_ant.
        CLEAR: zdoc_memo_protoc.
      WHEN c_a.
        EXIT.
    ENDCASE.

  ELSE.
    vg_dynnr_000 = vg_dynnr_ant.
    CLEAR: zdoc_memo_protoc.
  ENDIF.

ENDFORM.                    " SAIR_PROTOCOLO_VINCULACAO

*&---------------------------------------------------------------------*
*&      Form  SALVAR_PROTOCOLO_VINCULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar_protocolo_vinculacao .

  DATA: wa_mm_protocolos TYPE zdoc_memo_pro_me,
        it_mm_protocolos TYPE TABLE OF zdoc_memo_pro_me INITIAL SIZE 0 WITH HEADER LINE.

  CHECK NOT vg_alterou_protocolo IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mm_protocolos
    FROM zdoc_memo_pro_me
   WHERE nr_protocolo EQ zdoc_memo_protoc-nr_protocolo.

  IF sy-subrc IS INITIAL.
    LOOP AT it_mm_protocolos INTO wa_mm_protocolos.
      UPDATE zdoc_memorando SET status = c_i
       WHERE nr_memorando EQ wa_mm_protocolos-nr_memorando.
    ENDLOOP.
    DELETE FROM zdoc_memo_pro_me WHERE nr_protocolo EQ zdoc_memo_protoc-nr_protocolo.
  ENDIF.

  LOOP AT it_prot_memo INTO wa_prot_memo.
    MOVE-CORRESPONDING wa_prot_memo TO wa_mm_protocolos.
    MODIFY zdoc_memo_pro_me FROM wa_mm_protocolos.

    UPDATE zdoc_memorando SET status = c_t
     WHERE nr_memorando EQ wa_mm_protocolos-nr_memorando.
  ENDLOOP.

  COMMIT WORK.

  CLEAR: vg_alterou_protocolo.

ENDFORM.                    " SALVAR_PROTOCOLO_VINCULACAO

*&---------------------------------------------------------------------*
*&      Form  POPULA_MEMORANDOS_VINCULACAO
*&---------------------------------------------------------------------*
*       Procedimento de visualização de memorandos livres ao protocolo
*----------------------------------------------------------------------*
FORM popula_memorandos_vinculacao  USING vg_protocolo TYPE z_protocolo.

  DATA: wa_protoc      TYPE zdoc_memo_protoc,
        it_memorando3  TYPE TABLE OF zdoc_memorando   INITIAL SIZE 0 WITH HEADER LINE,
        it_memorando2  TYPE TABLE OF zdoc_memorando   INITIAL SIZE 0 WITH HEADER LINE,
        it_memo_notas  TYPE TABLE OF zdoc_memo_nota   INITIAL SIZE 0 WITH HEADER LINE,
        it_memo_notax  TYPE TABLE OF zdoc_memo_nota   INITIAL SIZE 0 WITH HEADER LINE,
        it_memo_nf_ex  TYPE TABLE OF zdoc_memo_nf_exp INITIAL SIZE 0 WITH HEADER LINE,
        it_memo_nf_ax  TYPE TABLE OF zdoc_memo_nf_exp INITIAL SIZE 0 WITH HEADER LINE,
        it_emissor     TYPE TABLE OF zmemo_cliente    INITIAL SIZE 0 WITH HEADER LINE,
        it_remetente   TYPE TABLE OF zmemo_cliente    INITIAL SIZE 0 WITH HEADER LINE,
        it_produto     TYPE TABLE OF zmemo_produto    INITIAL SIZE 0 WITH HEADER LINE,
        wa_memorandos  TYPE zdoc_memorando,
        wa_memo_notas  TYPE zdoc_memo_nota,
        wa_memo_nf_ex  TYPE zdoc_memo_nf_exp,
        wa_produto     TYPE zmemo_produto,
        wa_emissor     TYPE zmemo_cliente,
        dados_clin     TYPE kna1,
        dados_forn     TYPE lfa1,
        vg_menge       TYPE j_1bnetqty.

  DATA: vg_cpf         TYPE c LENGTH 14,
        vg_cnpj        TYPE c LENGTH 18.

  CLEAR: it_memorando3[], it_memorando2[], it_memo_notas[], it_memo_nf_ex[],
         it_memorandos[], it_memo_nf_ax[], it_emissor[],    it_produto[].

  "Selecionar Protocolo
  SELECT SINGLE * INTO wa_protoc
    FROM zdoc_memo_protoc
   WHERE nr_protocolo EQ vg_protocolo.

  CHECK sy-subrc IS INITIAL.

  "Selecionar memorandos não vinculados
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_memorando3
    FROM zdoc_memorando AS me
   WHERE NOT EXISTS ( SELECT * FROM zdoc_memo_pro_me AS pt WHERE pt~nr_memorando EQ me~nr_memorando ).

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_memo_notas
    FROM zdoc_memo_nota
     FOR ALL ENTRIES IN it_memorando3
   WHERE nr_memorando EQ it_memorando3-nr_memorando
     AND bukrs        EQ wa_protoc-empresa_de
     AND branch       EQ wa_protoc-filial_de.

  CHECK sy-subrc IS INITIAL.

  MOVE it_memo_notas[] TO it_memo_notax[].

  SORT it_memo_notax BY nr_memorando.
  DELETE ADJACENT DUPLICATES FROM it_memo_notax COMPARING nr_memorando.

  LOOP AT it_memorando3 INTO wa_memorandos.
    READ TABLE it_memo_notax INTO wa_memo_notas WITH KEY nr_memorando = wa_memorandos-nr_memorando BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      APPEND wa_memorandos TO it_memorando2.
    ENDIF.
  ENDLOOP.

  CHECK NOT it_memorando2[] IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_memo_nf_ex
    FROM zdoc_memo_nf_exp
    FOR ALL ENTRIES IN it_memorando2
   WHERE nr_nota_exp EQ it_memorando2-nr_nota_exp.

  MOVE it_memo_nf_ex[] TO it_memo_nf_ax[].
  SORT it_memo_nf_ax BY emissor.
  DELETE ADJACENT DUPLICATES FROM it_memo_nf_ax COMPARING emissor.

  LOOP AT it_memo_nf_ax INTO wa_memo_nf_ex.
    CLEAR: dados_clin.
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro = wa_memo_nf_ex-emissor
        p_partype  = c_c
      CHANGING
        wa_info_c  = dados_clin.

    wa_emissor-cod  = wa_memo_nf_ex-emissor.
    wa_emissor-nome = dados_clin-name1.
    IF dados_clin-stkzn IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = dados_clin-stcd1
        IMPORTING
          output = vg_cnpj.
      wa_emissor-cnpj = vg_cnpj.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = dados_clin-stcd2
        IMPORTING
          output = vg_cpf.
      wa_emissor-cnpj = vg_cpf.
    ENDIF.
    APPEND wa_emissor TO it_emissor.
  ENDLOOP.

  MOVE it_memo_nf_ex[] TO it_memo_nf_ax[].
  SORT it_memo_nf_ax BY material.
  DELETE ADJACENT DUPLICATES FROM it_memo_nf_ax COMPARING material.

  SELECT matnr maktg INTO CORRESPONDING FIELDS OF TABLE it_produto
    FROM makt
     FOR ALL ENTRIES IN it_memo_nf_ax
   WHERE matnr EQ it_memo_nf_ax-material.

  MOVE it_memorando2[] TO it_memorando3[].
  SORT it_memorando3 BY remetente.
  DELETE ADJACENT DUPLICATES FROM it_memorando3 COMPARING remetente.

  LOOP AT it_memorando3 INTO wa_memorandos.
    CLEAR: dados_forn.
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_memorandos-remetente
        p_partype    = c_v
      CHANGING
        wa_info_part = dados_forn.

    wa_emissor-cod  = wa_memorandos-remetente.
    wa_emissor-nome = dados_forn-name1.
    IF dados_forn-stkzn IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = dados_forn-stcd1
        IMPORTING
          output = vg_cnpj.
      wa_emissor-cnpj = vg_cnpj.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = dados_forn-stcd2
        IMPORTING
          output = vg_cpf.
      wa_emissor-cnpj = vg_cpf.
    ENDIF.
    APPEND wa_emissor TO it_remetente.
  ENDLOOP.

  SORT it_emissor BY cod.
  SORT it_remetente BY cod.
  SORT it_produto BY matnr.
  SORT it_memo_nf_ex BY nr_nota_exp.
  SORT it_memo_notas BY nr_memorando.

  LOOP AT it_memorando2 INTO wa_memorandos.

    CLEAR: wa_memorando.
    vg_menge = 0.

    MOVE-CORRESPONDING wa_memorandos TO wa_memorando.

    READ TABLE it_memo_nf_ex INTO wa_memo_nf_ex WITH KEY nr_nota_exp = wa_memorando-nr_nota_exp.
    IF sy-subrc IS INITIAL.
      wa_memorando-emissor     = wa_memo_nf_ex-emissor.
      wa_memorando-material    = wa_memo_nf_ex-material.
      wa_memorando-numero_nota = wa_memo_nf_ex-numero_nota.
    ENDIF.

    READ TABLE it_emissor INTO wa_emissor WITH KEY cod = wa_memorando-emissor BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_memorando-emissorn    = wa_emissor-nome.
      wa_memorando-emissorcnpj = wa_emissor-cnpj.
    ENDIF.

    READ TABLE it_remetente INTO wa_emissor WITH KEY cod = wa_memorando-remetente BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_memorando-remetenten    = wa_emissor-nome.
      wa_memorando-remetentecnpj = wa_emissor-cnpj.
    ENDIF.

    READ TABLE it_produto INTO wa_produto WITH KEY matnr = wa_memorando-material BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_memorando-materialn = wa_produto-maktg.
    ENDIF.

    LOOP AT it_memo_notas INTO wa_memo_notas WHERE nr_memorando EQ wa_memorando-nr_memorando.
      vg_menge = vg_menge + wa_memo_notas-menge.
    ENDLOOP.

    IF NOT wa_memorando-cancelado IS INITIAL.
      wa_memorando-icone = icon_cancel.
    ELSEIF wa_memorando-quantidade_memo EQ vg_menge.
      wa_memorando-icone = icon_allow.
    ELSE.
      wa_memorando-icone = icon_reject.
    ENDIF.

    APPEND wa_memorando TO it_memorandos.

  ENDLOOP.

  SORT it_memorandos BY nr_memorando.

ENDFORM.                    " POPULA_MEMORANDOS_VINCULACAO

*&---------------------------------------------------------------------*
*&      Form  POPULA_MEMORANDOS_VINCULADOS
*&---------------------------------------------------------------------*
*       Procedimento de visualização de memorandos vinculados ao protocolo
*----------------------------------------------------------------------*
FORM popula_memorandos_vinculados  USING  vg_protocolo TYPE z_protocolo.

  DATA: itl_memo_protocolo TYPE TABLE OF zdoc_memo_pro_me INITIAL SIZE 0 WITH HEADER LINE,
        itl_memorandos     TYPE TABLE OF zdoc_memorando   INITIAL SIZE 0 WITH HEADER LINE,
        itl_memorandos_aux TYPE TABLE OF zdoc_memorando   INITIAL SIZE 0 WITH HEADER LINE,
        itl_notas_expt     TYPE TABLE OF zdoc_memo_nf_exp INITIAL SIZE 0 WITH HEADER LINE,
        itl_memo_nf_ax     TYPE TABLE OF zdoc_memo_nf_exp INITIAL SIZE 0 WITH HEADER LINE,
        wal_memo_protocolo TYPE zdoc_memo_pro_me,
        wal_memorandos     TYPE zdoc_memorando,
        wal_notas_expt     TYPE zdoc_memo_nf_exp,
        it_emissor         TYPE TABLE OF zmemo_cliente    INITIAL SIZE 0 WITH HEADER LINE,
        it_remetente       TYPE TABLE OF zmemo_cliente    INITIAL SIZE 0 WITH HEADER LINE,
        it_produto         TYPE TABLE OF zmemo_produto    INITIAL SIZE 0 WITH HEADER LINE,
        dados_clin         TYPE kna1,
        dados_forn         TYPE lfa1,
        wa_produto         TYPE zmemo_produto,
        wa_emissor         TYPE zmemo_cliente.

  DATA: vg_cpf         TYPE c LENGTH 14,
        vg_cnpj        TYPE c LENGTH 18.

  CLEAR: it_prot_memo[].

  "Memorandos Vinculados
  SELECT * INTO CORRESPONDING FIELDS OF TABLE itl_memo_protocolo
    FROM zdoc_memo_pro_me
   WHERE nr_protocolo EQ vg_protocolo.
  CHECK NOT itl_memo_protocolo[] IS INITIAL.

  "Informações de Memorando
  SELECT * INTO CORRESPONDING FIELDS OF TABLE itl_memorandos
    FROM zdoc_memorando
    FOR ALL ENTRIES IN itl_memo_protocolo
   WHERE nr_memorando EQ itl_memo_protocolo-nr_memorando.
  CHECK NOT itl_memorandos[] IS INITIAL.

  "Notas de Exportação de Memorandos
  SELECT * INTO CORRESPONDING FIELDS OF TABLE itl_notas_expt
    FROM zdoc_memo_nf_exp
     FOR ALL ENTRIES IN itl_memorandos
   WHERE nr_nota_exp EQ itl_memorandos-nr_nota_exp.
  CHECK NOT itl_notas_expt[] IS INITIAL.

  MOVE itl_notas_expt[] TO itl_memo_nf_ax[].
  SORT itl_memo_nf_ax BY emissor.
  DELETE ADJACENT DUPLICATES FROM itl_memo_nf_ax COMPARING emissor.

  LOOP AT itl_memo_nf_ax INTO wal_notas_expt.
    CLEAR: dados_clin.
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro = wal_notas_expt-emissor
        p_partype  = c_c
      CHANGING
        wa_info_c  = dados_clin.

    wa_emissor-cod  = wal_notas_expt-emissor.
    wa_emissor-nome = dados_clin-name1.
    IF dados_clin-stkzn IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = dados_clin-stcd1
        IMPORTING
          output = vg_cnpj.
      wa_emissor-cnpj = vg_cnpj.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = dados_clin-stcd2
        IMPORTING
          output = vg_cpf.
      wa_emissor-cnpj = vg_cpf.
    ENDIF.
    APPEND wa_emissor TO it_emissor.
  ENDLOOP.

  MOVE itl_notas_expt[] TO itl_memo_nf_ax[].
  SORT itl_memo_nf_ax BY material.
  DELETE ADJACENT DUPLICATES FROM itl_memo_nf_ax COMPARING material.

  SELECT matnr maktg INTO CORRESPONDING FIELDS OF TABLE it_produto
    FROM makt
     FOR ALL ENTRIES IN itl_memo_nf_ax
   WHERE matnr EQ itl_memo_nf_ax-material.

  MOVE itl_memorandos[] TO itl_memorandos_aux[].
  SORT itl_memorandos_aux BY remetente.
  DELETE ADJACENT DUPLICATES FROM itl_memorandos_aux COMPARING remetente.

  LOOP AT itl_memorandos_aux INTO wal_memorandos.
    CLEAR: dados_forn.
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wal_memorandos-remetente
        p_partype    = c_v
      CHANGING
        wa_info_part = dados_forn.

    wa_emissor-cod  = wal_memorandos-remetente.
    wa_emissor-nome = dados_forn-name1.
    IF dados_forn-stkzn IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = dados_forn-stcd1
        IMPORTING
          output = vg_cnpj.
      wa_emissor-cnpj = vg_cnpj.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = dados_forn-stcd2
        IMPORTING
          output = vg_cpf.
      wa_emissor-cnpj = vg_cpf.
    ENDIF.
    APPEND wa_emissor TO it_remetente.
  ENDLOOP.

  SORT itl_memo_protocolo BY nr_memorando.
  SORT itl_memorandos BY nr_memorando.
  SORT itl_notas_expt BY nr_nota_exp.
  SORT it_emissor BY cod.
  SORT it_remetente BY cod.
  SORT it_produto BY matnr.

  LOOP AT itl_memo_protocolo INTO wal_memo_protocolo.

    CLEAR: wa_prot_memo.

    MOVE-CORRESPONDING wal_memo_protocolo TO wa_prot_memo.

    READ TABLE itl_memorandos INTO wal_memorandos WITH KEY nr_memorando = wal_memo_protocolo-nr_memorando BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING wal_memorandos TO wa_prot_memo.
      READ TABLE itl_notas_expt INTO wal_notas_expt WITH KEY nr_nota_exp = wa_prot_memo-nr_nota_exp.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING wal_notas_expt TO wa_prot_memo.
      ENDIF.
    ENDIF.

    READ TABLE it_emissor INTO wa_emissor WITH KEY cod = wa_prot_memo-emissor BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_prot_memo-emissorn    = wa_emissor-nome.
      wa_prot_memo-emissorcnpj = wa_emissor-cnpj.
    ENDIF.

    READ TABLE it_remetente INTO wa_emissor WITH KEY cod = wa_prot_memo-remetente BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_prot_memo-remetenten    = wa_emissor-nome.
      wa_prot_memo-remetentecnpj = wa_emissor-cnpj.
    ENDIF.

    READ TABLE it_produto INTO wa_produto WITH KEY matnr = wa_prot_memo-material BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_prot_memo-materialn = wa_produto-maktg.
    ENDIF.

    wa_prot_memo-icone = icon_allow.

    APPEND wa_prot_memo TO it_prot_memo.

  ENDLOOP.


ENDFORM.                    " POPULA_MEMORANDOS_VINCULADOS

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_MEMO_PROT
*&---------------------------------------------------------------------*
*       Verifica de foi selecionado algum memorando
*----------------------------------------------------------------------*
FORM verifica_selecao_memo_prot  USING vg_slecionado TYPE sy-subrc
                                       vg_desvincula TYPE c.

  IF vg_desvincula IS INITIAL.
    READ TABLE it_memorandos INTO wa_memorando WITH KEY mark = c_x.
  ELSE.
    READ TABLE it_prot_memo INTO wa_prot_memo WITH KEY mark = c_x.
  ENDIF.
  vg_slecionado = sy-subrc.

  IF NOT vg_slecionado IS INITIAL.
    MESSAGE text-e12 TYPE c_e DISPLAY LIKE c_s.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_MEMO_PROT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_MEMORANDO_PROTOCOLO
*&---------------------------------------------------------------------*
*       Procedimento de Vinculação de Memorando em Protocolo
*----------------------------------------------------------------------*
FORM vincular_memorando_protocolo .

  DATA: vg_slecionado TYPE sy-subrc.
  PERFORM verifica_selecao_memo_prot USING vg_slecionado space.

  CHECK vg_slecionado IS INITIAL.

  vg_alterou_protocolo = c_x.

  LOOP AT it_memorandos INTO wa_memorando WHERE mark = c_x AND icone NE icon_reject.
    CLEAR: wa_memorando-mark, wa_prot_memo.
    MOVE-CORRESPONDING wa_memorando TO wa_prot_memo.
    wa_prot_memo-nr_protocolo    = zdoc_memo_protoc-nr_protocolo.
    APPEND wa_prot_memo TO it_prot_memo.
  ENDLOOP.

  DELETE it_memorandos WHERE mark = c_x AND icone NE icon_reject.

ENDFORM.                    " VINCULAR_MEMORANDO_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Form  DESVINCULA_MEMORANDO_PROTOCOLO
*&---------------------------------------------------------------------*
*       Procedimento de Desvinculação de Memorando em Protocolo
*----------------------------------------------------------------------*
FORM desvincula_memorando_protocolo .

  DATA: vg_slecionado TYPE sy-subrc.
  PERFORM verifica_selecao_memo_prot USING vg_slecionado c_x.

  CHECK vg_slecionado IS INITIAL.

  vg_alterou_protocolo = c_x.

  LOOP AT it_prot_memo INTO wa_prot_memo WHERE mark = c_x.
    CLEAR: wa_prot_memo-mark.
    MOVE-CORRESPONDING wa_prot_memo TO wa_memorando.
    APPEND wa_memorando TO it_memorandos.
  ENDLOOP.

  DELETE it_prot_memo WHERE mark = c_x.

ENDFORM.                    " DESVINCULA_MEMORANDO_PROTOCOLO

*&---------------------------------------------------------------------*
*&      Module  STATUS_8052  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8052 OUTPUT.

  IF NOT zdoc_memo_protoc-dt_recibo IS INITIAL.
    LOOP AT SCREEN.
      IF ( screen-name EQ 'BTMMV' ) OR ( screen-name EQ 'BTMMD' ).
        screen-input  = c_0.
        screen-output = c_1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_8052  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VALIDA_MEMO_VINCULADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM valida_memo_vinculados  USING  vg_protocolo TYPE z_protocolo.

  DATA: wa_zdoc_memo_pro_me TYPE TABLE OF zdoc_memo_pro_me,
        wa_zdoc_memo_protoc TYPE zdoc_memo_protoc.

  SELECT * INTO TABLE wa_zdoc_memo_pro_me
    FROM zdoc_memo_pro_me
   WHERE nr_protocolo EQ vg_protocolo.

  CHECK sy-subrc IS INITIAL.

  MESSAGE e000 WITH 'Protocolo:' vg_protocolo 'possui memorandos vinculados' DISPLAY LIKE c_s.

ENDFORM.                    " VALIDA_MEMO_VINCULADOS

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_CABE_PROTOC'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_cabe_protoc_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_protocolos LINES tab_cabe_protoc-lines.
ENDMODULE.                    "TAB_CABE_PROTOC_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_CABE_PROTOC'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MARK TABLE
MODULE tab_cabe_protoc_mark INPUT.
  DATA: g_tab_cabe_protoc_wa2 LIKE LINE OF it_protocolos.
  IF tab_cabe_protoc-line_sel_mode = 1
  AND it_protocolos-mark = 'X'.
    LOOP AT it_protocolos INTO g_tab_cabe_protoc_wa2
      WHERE mark = 'X'.
      g_tab_cabe_protoc_wa2-mark = ''.
      MODIFY it_protocolos
        FROM g_tab_cabe_protoc_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_protocolos
    INDEX tab_cabe_protoc-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_CABE_PROTOC_MARK INPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_VINC_MEMO'. DO NOT CHANGE THIS LIN
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_vinc_memo_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_memorandos LINES tab_vinc_memo-lines.
ENDMODULE.                    "TAB_VINC_MEMO_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_VINC_MEMO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tab_vinc_memo_mark INPUT.
  DATA: g_tab_vinc_memo_wa2 LIKE LINE OF it_memorandos.
  IF tab_vinc_memo-line_sel_mode = 1
  AND it_memorandos-mark = 'X'.
    LOOP AT it_memorandos INTO g_tab_vinc_memo_wa2
      WHERE mark = 'X'.
      g_tab_vinc_memo_wa2-mark = ''.
      MODIFY it_memorandos
        FROM g_tab_vinc_memo_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_memorandos
    INDEX tab_vinc_memo-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_VINC_MEMO_MARK INPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_MEMO_VINC'. DO NOT CHANGE THIS LIN
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_memo_vinc_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_prot_memo LINES tab_memo_vinc-lines.
ENDMODULE.                    "TAB_MEMO_VINC_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_MEMO_VINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tab_memo_vinc_mark INPUT.
  DATA: g_tab_memo_vinc_wa2 LIKE LINE OF it_prot_memo.
  IF tab_memo_vinc-line_sel_mode = 1
  AND it_prot_memo-mark = 'X'.
    LOOP AT it_prot_memo INTO g_tab_memo_vinc_wa2
      WHERE mark = 'X'.
      g_tab_memo_vinc_wa2-mark = ''.
      MODIFY it_prot_memo
        FROM g_tab_memo_vinc_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_prot_memo
    INDEX tab_memo_vinc-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_MEMO_VINC_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_MEMO_VINC'. DO NOT CHANGE THIS LINE
*&SPWIZARD: PROCESS USER COMMAND
MODULE tab_memo_vinc_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TAB_MEMO_VINC'
                              'IT_PROT_MEMO'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TAB_MEMO_VINC_USER_COMMAND INPUT

*&---------------------------------------------------------------------*
*&      Form  EMITIR_PROTOLOCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PROTOCOLO  text
*----------------------------------------------------------------------*
FORM emitir_protoloco  USING   vg_protocolo TYPE zmemo_protocolo.

  TYPES: BEGIN OF ty_cidade.
  TYPES:   region	TYPE regio,
           text	  TYPE text60.
  TYPES: END OF ty_cidade.

  DATA: it_memo_memo  TYPE TABLE OF zdoc_memo_proto_m INITIAL SIZE 0 WITH HEADER LINE.

  DATA: vl_formname   TYPE tdsfname  ,
        vl_name       TYPE rs38l_fnam,
        wa_memo_proto TYPE zdoc_memo_proto,
        wa_memo_memo  TYPE zdoc_memo_proto_m ,
        wa_t247       TYPE t247,
        wa_lfa1       TYPE lfa1,
        wa_cidade     TYPE ty_cidade,
        vg_cidade     TYPE string,
        vg_qtd        TYPE i,
        vg_quantidade TYPE c LENGTH 13,
        p_parceiro    TYPE j_1bparid.

  PERFORM popula_memorandos_vinculados USING vg_protocolo-nr_protocolo.

  MOVE-CORRESPONDING vg_protocolo TO wa_memo_proto.

  SHIFT wa_memo_proto-nm_emissor    LEFT DELETING LEADING space.
  SHIFT wa_memo_proto-nm_empresa_em LEFT DELETING LEADING space.
  SHIFT wa_memo_proto-nm_filial_em  LEFT DELETING LEADING space.
  SHIFT wa_memo_proto-nm_destino    LEFT DELETING LEADING space.
  SHIFT wa_memo_proto-nm_empresa_de LEFT DELETING LEADING space.
  SHIFT wa_memo_proto-nm_filial_de  LEFT DELETING LEADING space.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = sy-langu
      month = vg_protocolo-dt_protocolo+4(2)
    IMPORTING
      t247  = wa_t247.

  CONCATENATE vg_protocolo-dt_protocolo+6(2) 'de' wa_t247-ltx 'de' vg_protocolo-dt_protocolo(4) INTO wa_memo_proto-nm_local_origem SEPARATED BY space.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vg_protocolo-filial_em
    IMPORTING
      output = p_parceiro.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = p_parceiro
      p_partype    = c_v
    CHANGING
      wa_info_part = wa_lfa1.

  IF NOT wa_lfa1-txjcd IS INITIAL.

    SELECT SINGLE
           cep~region
           cid~text
      INTO wa_cidade
      FROM j_1btreg_city AS cep
      INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                               AND cid~taxjurcode EQ cep~taxjurcode
     WHERE cep~taxjurcode EQ wa_lfa1-txjcd.

    IF sy-subrc IS INITIAL.
      CONCATENATE wa_cidade-text '-' wa_cidade-region INTO vg_cidade SEPARATED BY space.
      CONCATENATE vg_cidade ',' INTO vg_cidade.
      CONCATENATE vg_cidade wa_memo_proto-nm_local_origem INTO wa_memo_proto-nm_local_origem SEPARATED BY space.
    ENDIF.

  ENDIF.

  "Agrupa Itens
  LOOP AT it_prot_memo INTO wa_prot_memo.
    MOVE-CORRESPONDING wa_prot_memo TO wa_memo_memo.
    vg_qtd = wa_memo_memo-quantidade_memo.
    WRITE vg_qtd TO vg_quantidade.
    SHIFT vg_quantidade LEFT DELETING LEADING space.
    wa_memo_memo-quantidade = vg_quantidade.
    APPEND wa_memo_memo TO it_memo_memo.
  ENDLOOP.

  "Gerar Impressão
  vl_formname = 'ZMEMOPROTOCOLO'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      p_info           = wa_memo_proto
    TABLES
      it_memo          = it_memo_memo
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " EMITIR_PROTOLOCO

*&---------------------------------------------------------------------*
*&      Form  CANCELA_MEMORANDO_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cancela_memorando_protocolo .

  DATA: vg_slecionado TYPE sy-subrc,
        answer        TYPE c LENGTH 1,
        vg_verifica_selecao TYPE sy-subrc,
        wa_doc_memo   TYPE zdoc_memorando,
        vg_tabix      TYPE sy-tabix.

  PERFORM verifica_selecao_memo_prot USING vg_slecionado c_x.

  CHECK vg_slecionado IS INITIAL.

  vg_alterou_protocolo = c_x.

  LOOP AT it_prot_memo INTO wa_prot_memo WHERE mark = c_x.

    vg_tabix  = sy-tabix.

    CLEAR: wa_prot_memo-mark.

    MODIFY it_prot_memo FROM wa_prot_memo INDEX vg_tabix TRANSPORTING mark.

    SELECT SINGLE * INTO wa_doc_memo
      FROM zdoc_memorando
     WHERE nr_memorando EQ wa_prot_memo-nr_memorando.

    IF NOT wa_doc_memo-cancelado IS INITIAL.
      MESSAGE e068 WITH wa_doc_memo-numero_memo DISPLAY LIKE c_s.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = text-001
        textline1 = text-004
        textline2 = text-005
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN c_j.
        CLEAR vg_verifica_selecao.
      WHEN c_n.
        vg_verifica_selecao = 4.
      WHEN c_a.
        vg_verifica_selecao = 4.
    ENDCASE.

    IF vg_verifica_selecao IS INITIAL.
      SELECT SINGLE * INTO wa_memorando
        FROM zdoc_memorando
       WHERE nr_memorando EQ wa_memorando-nr_memorando.

      wa_memorando-cancelado = c_x.
      DELETE FROM zdoc_memo_nota   WHERE nr_memorando EQ wa_memorando-nr_memorando.
      DELETE FROM zdoc_memo_nota_s WHERE nr_memorando EQ wa_memorando-nr_memorando.
      MODIFY zdoc_memorando FROM wa_memorando.
      COMMIT WORK.
      CLEAR: wa_memorando.
    ENDIF.

  ENDLOOP.

  PERFORM popula_memorandos_vinculacao USING zdoc_memo_protoc-nr_protocolo.
  PERFORM popula_memorandos_vinculados USING zdoc_memo_protoc-nr_protocolo.

ENDFORM.                    " CANCELA_MEMORANDO_PROTOCOLO
