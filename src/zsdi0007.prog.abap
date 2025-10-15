*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0007                                                *
* Descrição  : Vínculo Documento de Exportação                         *
* Módulo     : SD                                Transação: ZSDT0014   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 23/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0007 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: j_1bnfdoc, znom_reme_notas.

*----------------------------------------------------------------------*
* Bibliotecas
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_cabec,
         bukrs       TYPE t001-bukrs,
         butxt       TYPE t001-butxt,
         werks       TYPE t001w-werks,
         gsber       TYPE vbap-gsber,
         name1       TYPE t001w-name1,
         lgort       TYPE t001l-lgort,
         lgobe       TYPE t001l-lgobe,
         re          TYPE char14,
         id_due      TYPE zsdt0170-id_due,
         numero_due  TYPE zsdt0170-numero_due,
         dde         TYPE zdde-nr_dde,
         id_dde      TYPE zdde-id_dde,
         navio       TYPE zreg_exportacao-id_nomeacao_tran,
         desc        TYPE znom_transporte-ds_nome_transpor,
         exp         TYPE zreg_exportacao-id_registro_expo,
         cd_material TYPE zreg_exportacao-cd_material,
       END OF type_cabec.

TYPES: BEGIN OF ty_notas.
         INCLUDE STRUCTURE j_1bnfdoc.
         TYPES:   itmnum TYPE j_1bitmnum,
         charg  TYPE charg_d,
         meins  TYPE j_1bnetunt,
         menge  TYPE j_1bnetqty,
         saldo  TYPE j_1bnetqty,
         vbeln  TYPE vbeln_vl,
         nome   TYPE name1_gp,
         fiscal TYPE c LENGTH 18,
         uf     TYPE regio,
         mark   TYPE c LENGTH 1,
         status TYPE c LENGTH 4,
         matnr  TYPE matnr.
TYPES: END OF ty_notas.

TYPES: BEGIN OF ty_notas_vinculas.
         INCLUDE STRUCTURE zdoc_nf_produtor.
         TYPES:   docdat TYPE j_1bdocdat,
         nfenum TYPE j_1bnfnum9,
         charg  TYPE charg_d,
         parid  TYPE j_1bparid,
         partyp TYPE j_1bpartyp,
         nome   TYPE name1_gp,
         fiscal TYPE c LENGTH 18,
         uf     TYPE regio,
         mark   TYPE c LENGTH 1,
         meins  TYPE j_1bnetunt.
TYPES: END OF ty_notas_vinculas.

TYPES: BEGIN OF ty_grupo,
         parid  TYPE j_1bparid,
         fiscal TYPE c LENGTH 18,
         uf     TYPE regio,
         nome   TYPE name1_gp,
         volume TYPE j_1bnetqty,
         meins  TYPE j_1bnetunt.
TYPES: END OF ty_grupo.

TYPES: BEGIN OF ty_reme_nota,
         id_nomeacao_tran TYPE znom_reme_notas-id_nomeacao_tran,
         id_empresa       TYPE znom_reme_notas-id_empresa,
         id_filial        TYPE znom_reme_notas-id_filial,
         id_material      TYPE znom_reme_notas-id_material,
         id_remetente     TYPE znom_reme_notas-id_remetente,
         docnum           TYPE znom_reme_notas-docnum,
         itmnum           TYPE znom_reme_notas-itmnum,
         id_unidade       TYPE znom_reme_notas-id_unidade,
         nr_quantidade    TYPE znom_reme_notas-nr_quantidade,
       END OF ty_reme_nota,

       BEGIN OF ty_remetente,
         id_nomeacao_tran TYPE znom_remetente-id_nomeacao_tran,
         id_empresa       TYPE znom_remetente-id_empresa,
         id_filial        TYPE znom_remetente-id_filial,
         id_material      TYPE znom_remetente-id_material,
         id_remetente     TYPE znom_remetente-id_remetente,
         id_unidade       TYPE znom_remetente-id_unidade,
         nr_programada    TYPE znom_remetente-nr_programada,
         nr_parte_empresa TYPE znom_remetente-nr_parte_empresa,
         docnum_rt        TYPE znom_remetente-docnum_rt,
         nr_ordem         TYPE znom_remetente-nr_ordem,
       END OF ty_remetente,

       BEGIN OF ty_nom_tran,
         id_nomeacao_tran TYPE znom_prog_reme-id_nomeacao_tran,
         id_empresa       TYPE znom_prog_reme-id_empresa,
         id_filial        TYPE znom_prog_reme-id_filial,
         id_material      TYPE znom_prog_reme-id_material,
       END OF ty_nom_tran.

DATA: BEGIN OF t_conhec_a OCCURS 0.
        INCLUDE TYPE znom_conhec.
        DATA:    marc  TYPE char1,
        id    TYPE zid_doc,
        item  TYPE zid_item,
        landx TYPE landx.
DATA:  END  OF t_conhec_a.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_conhec_v     LIKE TABLE OF t_conhec_a,
      t_full         LIKE TABLE OF t_conhec_a,
      w_notas        TYPE ty_notas,
      t_notas        TYPE TABLE OF ty_notas          INITIAL SIZE 0 WITH HEADER LINE,
      t_notas_aux    TYPE TABLE OF ty_notas          INITIAL SIZE 0 WITH HEADER LINE,
      w_vincu        TYPE ty_notas_vinculas,
      t_vincu        TYPE TABLE OF ty_notas_vinculas INITIAL SIZE 0 WITH HEADER LINE,
      t_desvi        TYPE TABLE OF ty_notas_vinculas INITIAL SIZE 0 WITH HEADER LINE,
      t_temp         TYPE TABLE OF ty_notas_vinculas INITIAL SIZE 0 WITH HEADER LINE,
      t_vincu_aux    TYPE TABLE OF ty_notas_vinculas INITIAL SIZE 0 WITH HEADER LINE,
      tw_nf_produtor TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
      tw_exp         TYPE TABLE OF zdoc_exp         WITH HEADER LINE,
      tw_prog_reme   TYPE TABLE OF znom_prog_reme WITH HEADER LINE,
      w_grupo        TYPE ty_grupo,
      t_reme_nota    TYPE TABLE OF ty_reme_nota,
      w_reme_nota    TYPE ty_reme_nota,
      t_remetente    TYPE TABLE OF ty_remetente,
      w_remetente    TYPE ty_remetente,
      t_grupo        TYPE TABLE OF ty_grupo,
      t_nom_tran     TYPE TABLE OF ty_nom_tran,
      w_nom_tran     TYPE ty_nom_tran.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: v_botao     TYPE char1,
      v_nu_dco    TYPE znu_dco,
      vg_matnr    TYPE matnr,
      vg_matkl    TYPE matkl,
      vg_menge    TYPE j_1bnetqty, "Quantidade da Nota Fiscal de Exportação
      vg_menge_s  TYPE j_1bnetqty, "Quantidade Saldo a Vincular
      vg_menge_v  TYPE j_1bnetqty, "Quantidade vinculada
      vg_menge_l  TYPE j_1bnetqty, "Quantidade livre para vincular
      vg_menge_qp TYPE j_1bnetqty. "Quantidade p/ vinculação parcial de nota única

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cabec     TYPE type_cabec,
      s_prod      TYPE t001w,
      s_produtor  TYPE lfa1,

      s_conhec_a  LIKE LINE OF t_conhec_a,
      s_conhec_v  LIKE LINE OF t_conhec_a,
      wnfprodutor TYPE zdoc_nf_produtor.

DATA: wl_rem_not LIKE znom_reme_notas,
      wl_reme    LIKE znom_remetente.

CONTROLS: tc_avinc       TYPE TABLEVIEW USING SCREEN '0100',
          tc_vinc        TYPE TABLEVIEW USING SCREEN '0100',
          tab_notas      TYPE TABLEVIEW USING SCREEN '0002',
          tab_vinc       TYPE TABLEVIEW USING SCREEN '0003',
          tab_vinc_grupo TYPE TABLEVIEW USING SCREEN '0004'.

CONTROLS: vg_main100_tabstrip TYPE TABSTRIP.

DATA:     g_tab_notas_lines LIKE sy-loopc.
DATA:     g_tab_vinc_lines  LIKE sy-loopc.
DATA:     ok_code LIKE sy-ucomm.
DATA:     direcao LIKE sy-ucomm.

CONSTANTS:
  BEGIN OF tabs,
    tab1 LIKE sy-ucomm  VALUE 'TAB_VINC',
    tab2 LIKE sy-ucomm  VALUE 'TAB_GRUP',
  END   OF tabs.

DATA: vg_dynnr TYPE sy-dynnr VALUE '0003'.

PARAMETERS: p_tcode TYPE syst-tcode NO-DISPLAY.
PARAMETERS: v_vbeln TYPE likp-vbeln NO-DISPLAY.


START-OF-SELECTION.

  IF p_tcode IS NOT INITIAL.
    PERFORM z_verifica_rem.
    CALL SCREEN 0100.
  ENDIF.

  "DATA: V_TCODE  TYPE SYST-TCODE.

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
      DESCRIBE TABLE: t_notas    LINES tab_notas-lines,
                      t_vincu    LINES tab_vinc-lines.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.
  DATA: vl_answer TYPE c LENGTH 1.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'CANC' OR 'EXIT'.
*          IF V_VBELN IS NOT INITIAL.
*            CALL FUNCTION 'POPUP_TO_CONFIRM'
*              EXPORTING
*                TEXT_QUESTION  = TEXT-019
*              IMPORTING
*                ANSWER         = VL_ANSWER
*              EXCEPTIONS
*                TEXT_NOT_FOUND = 1
*                OTHERS         = 2.
*
*            IF SY-SUBRC <> 0.
*              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*            ENDIF.
*
*            CHECK VL_ANSWER EQ '1'.
*
*            REFRESH: T_CONHEC_A,
*                     T_CONHEC_V,
*                     T_REME_NOTA,
*                     T_REMETENTE,
*                     T_FULL.
*
*            CLEAR: S_CABEC,
*                   V_VBELN.
*
*            MODIFY SCREEN.
*          ELSE.
          LEAVE PROGRAM.
*          ENDIF.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  DATA: vg_automatico TYPE c LENGTH 1.

  CASE sy-ucomm.
    WHEN 'DESV'.
*     Desvincular
      PERFORM z_desvincular.
    WHEN 'BTNDES'.
      PERFORM z_desvincular_due.
    WHEN 'VINC'.
*     Vincular
      PERFORM z_vincular.
    WHEN 'SAVE'.
*     Salvar
      PERFORM z_save.
    WHEN 'NEW'.
    WHEN 'DEL'.
    WHEN 'BTPSQN'.
      direcao = ok_code.
*   Preenche Table Controls de Nota Fiscal do Produtor
      PERFORM z_preenche_controls_nf_prod.
    WHEN 'BTDELET'.
      PERFORM z_desvincula_nf_prod.
  ENDCASE.

  CASE ok_code.
    WHEN 'AUTO' OR 'VINCNF'.
      direcao = ok_code.
      IF ok_code EQ 'AUTO'.
        vg_automatico = 'X'.
      ELSE.
        CLEAR: vg_automatico.
      ENDIF.
      PERFORM vincular USING vg_automatico.
    WHEN 'DESVNF'.
      direcao = ok_code.
      PERFORM desvincular.
  ENDCASE.

  CLEAR: sy-ucomm, ok_code.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_VBELN  INPUT                                        *
*&---------------------------------------------------------------------*
*                                  Remessa                             *
*----------------------------------------------------------------------*
MODULE zm_vbeln INPUT.
* Verifica Remessa
  PERFORM z_verifica_rem.
ENDMODULE.                 " ZM_VBELN  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_REM                                           *
*&---------------------------------------------------------------------*
*                          Verifica Remessa                            *
*----------------------------------------------------------------------*
FORM z_verifica_rem.

  FREE: t_conhec_a, t_conhec_v, t_full, t_vincu, t_notas.
  CLEAR: s_cabec-bukrs, s_cabec-butxt, s_cabec-werks, s_cabec-name1, s_cabec-lgort,s_cabec-lgobe,
         s_cabec-re, s_cabec-dde, s_cabec-navio, s_prod-werks,s_produtor-lifnr.

  CLEAR: s_cabec, s_produtor.

  CHECK NOT v_vbeln IS INITIAL.

  SELECT SINGLE vbeln
    FROM likp
    INTO v_vbeln
  WHERE vbeln EQ v_vbeln.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-001.
  ELSE.
*   Preenche Dados Empresa/Centro/Depósito
    PERFORM z_preenche_emp.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
*   Prencher o número da Re para Remessa que já estajam vinculadas.
    PERFORM z_preenche_re.
*   Preenche os dados das Notas Vinculadas
    PERFORM seleciona_notas.
  ENDIF.
ENDFORM.                    " Z_VERIFICA_REM

*&---------------------------------------------------------------------*
*&      Module  ZM_RE  INPUT                                           *
*&---------------------------------------------------------------------*
*                                   RE                                 *
*----------------------------------------------------------------------*
MODULE zm_re INPUT.

  PERFORM z_verifica_re.

ENDMODULE.                 " ZM_RE  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_EMP                                           *
*&---------------------------------------------------------------------*
*             Preenche Dados Empresa/Centro/Depósito                   *
*----------------------------------------------------------------------*
FORM z_preenche_emp.

  DATA vl_vgbel TYPE lips-vgbel.

  SELECT SINGLE vgbel
    FROM lips
    INTO vl_vgbel
  WHERE  vbeln EQ v_vbeln.

  CHECK NOT vl_vgbel IS INITIAL.

  SELECT SINGLE werks gsber lgort
    FROM vbap
    INTO (s_cabec-werks, s_cabec-gsber, s_cabec-lgort)
  WHERE  vbeln EQ vl_vgbel.

  SELECT SINGLE bukrs_vf
    FROM vbak
    INTO s_cabec-bukrs
  WHERE  vbeln EQ vl_vgbel.

  IF NOT s_cabec-werks IS INITIAL.
    SELECT SINGLE name1
      FROM t001w
      INTO s_cabec-name1
    WHERE  werks EQ s_cabec-werks.
  ENDIF.

  IF s_prod-werks IS INITIAL.
    s_prod-werks = s_cabec-gsber.

    SELECT SINGLE * INTO s_prod
      FROM t001w
        WHERE werks EQ s_prod-werks.

  ENDIF.


  IF NOT s_cabec-lgort IS INITIAL.
    SELECT SINGLE lgobe
      FROM t001l
      INTO s_cabec-lgobe
    WHERE  lgort EQ s_cabec-lgort.
  ENDIF.

  IF NOT s_cabec-bukrs IS INITIAL.
    SELECT SINGLE butxt
      FROM t001
      INTO s_cabec-butxt
    WHERE  bukrs EQ s_cabec-bukrs.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_EMP

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_DDE                                           *
*&---------------------------------------------------------------------*
*                        Preenche Dados DDE/Navio                      *
*----------------------------------------------------------------------*
FORM z_preenche_dde.

  SELECT SINGLE id_registro_expo id_nomeacao_tran cd_material
    FROM zreg_exportacao
    INTO (s_cabec-exp, s_cabec-navio, s_cabec-cd_material)
   WHERE nr_registro_expo EQ s_cabec-re
     AND in_status_comex  NE 'X'.

  IF s_cabec-numero_due IS NOT INITIAL.
    SELECT SINGLE id_due id_nomeacao_tran
      INTO (s_cabec-id_due, s_cabec-navio)
      FROM zsdt0170
     WHERE numero_due EQ s_cabec-numero_due
       AND status      EQ '1'.
  ENDIF.

  IF NOT s_cabec-exp IS INITIAL.
    SELECT SINGLE id_dde
      FROM zdde_aplicacao
      INTO s_cabec-id_dde
    WHERE  id_registro_expo EQ s_cabec-exp.
  ENDIF.

  IF NOT s_cabec-navio IS INITIAL.
    SELECT SINGLE ds_nome_transpor
      FROM znom_transporte
      INTO s_cabec-desc
    WHERE  id_nomeacao_tran EQ s_cabec-navio.
  ENDIF.

  IF NOT s_cabec-id_dde IS INITIAL.
    SELECT SINGLE nr_dde
      FROM zdde
      INTO s_cabec-dde
    WHERE  id_dde EQ s_cabec-id_dde.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_DDE

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CONTROLS                                      *
*&---------------------------------------------------------------------*
*                        Preenche Table Controls                       *
*----------------------------------------------------------------------*
FORM z_preenche_controls.

  TYPES: BEGIN OF ty_pais,
           sign   TYPE char1,
           option TYPE  char2,
           low    TYPE land1,
           high   TYPE land1,
         END OF ty_pais.

  DATA: tl_rem_bl TYPE TABLE OF zdoc_rem_bl,
        tl_exp    TYPE TABLE OF zdoc_exp,
        sl_conhec LIKE LINE OF  t_conhec_a,
        sl_rem_bl TYPE zdoc_rem_bl,
        it_pais   TYPE TABLE OF ty_pais WITH HEADER LINE,
        it_t005t  TYPE TABLE OF t005t INITIAL SIZE 0 WITH HEADER LINE,
        wa_t005t  TYPE t005t,
        vg_tabix  TYPE sy-tabix.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full    .

  IF NOT s_cabec-navio IS INITIAL.

    SELECT *
      FROM znom_conhec
      INTO CORRESPONDING FIELDS OF TABLE t_conhec_a
     WHERE id_nomeacao_tran EQ s_cabec-navio.

    SORT t_conhec_a BY id_conhec        ASCENDING
                       id_nomeacao_tran ASCENDING.

    t_full[] = t_conhec_a[].

    LOOP AT t_conhec_a INTO sl_conhec.
      it_pais-sign   = 'I'.
      it_pais-option = 'EQ'.
      it_pais-low    = sl_conhec-sg_pais_destino.
      it_pais-high   = sl_conhec-sg_pais_destino.
      APPEND it_pais.
    ENDLOOP.

    SORT it_pais BY low.
    DELETE ADJACENT DUPLICATES FROM it_pais COMPARING low.

    IF NOT it_pais[] IS INITIAL.
      SELECT * INTO TABLE it_t005t
        FROM t005t
       WHERE spras EQ sy-langu
         AND land1 IN it_pais.
    ENDIF.

    LOOP AT t_conhec_a INTO sl_conhec.
      vg_tabix = sy-tabix.
      it_pais-low = sl_conhec-sg_pais_destino.
      READ TABLE it_t005t INTO wa_t005t WITH KEY land1 = it_pais-low.
      IF sy-subrc IS INITIAL.
        sl_conhec-landx = wa_t005t-landx.
        MODIFY t_conhec_a INDEX vg_tabix FROM sl_conhec TRANSPORTING landx.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF p_tcode IS NOT INITIAL.

    SELECT *  FROM zdoc_exp INTO TABLE tl_exp  WHERE vbeln  EQ v_vbeln.

  ELSE.

    IF ( v_vbeln     IS NOT INITIAL ) AND
       ( s_cabec-exp IS NOT INITIAL ).
      SELECT *
        FROM zdoc_exp INTO TABLE tl_exp
       WHERE vbeln            EQ v_vbeln
         AND id_registro_expo EQ s_cabec-exp.
    ENDIF.

    IF ( v_vbeln        IS NOT INITIAL ) AND
       ( s_cabec-id_due IS NOT INITIAL ).
      SELECT *
        FROM zdoc_exp INTO TABLE tl_exp
       WHERE vbeln  EQ v_vbeln
         AND id_due EQ s_cabec-id_due.
    ENDIF.
  ENDIF.

*  SELECT *
*    FROM ZDOC_EXP
*    INTO TABLE TL_EXP
*  WHERE  VBELN            EQ V_VBELN
*    AND  ID_REGISTRO_EXPO EQ S_CABEC-EXP.

  IF NOT tl_exp[] IS INITIAL.
    SELECT *
      FROM zdoc_rem_bl
      INTO TABLE tl_rem_bl
      FOR ALL ENTRIES IN tl_exp
    WHERE  id_doc_exp EQ tl_exp-id_doc_exp.
  ENDIF.

  LOOP AT tl_rem_bl INTO sl_rem_bl.
    sl_conhec-id_conhec        = sl_rem_bl-id_conhec.
    sl_conhec-id_nomeacao_tran = sl_rem_bl-id_nomeacao_tran.
    sl_conhec-dt_data          = sl_rem_bl-dt_data.
    sl_conhec-nr_qtde          = sl_rem_bl-nr_qtde.
    sl_conhec-nr_conhec        = sl_rem_bl-nr_conhec.
    sl_conhec-id               = sl_rem_bl-id_doc_exp.
    sl_conhec-item             = sl_rem_bl-id_doc_item.

    APPEND sl_conhec TO t_conhec_v.

    CLEAR: sl_rem_bl,
           sl_conhec.
  ENDLOOP.

  IF NOT t_conhec_v[] IS INITIAL.

    LOOP AT t_conhec_v INTO sl_conhec.
      READ TABLE t_conhec_a
        WITH KEY id_conhec        = sl_conhec-id_conhec
                 id_nomeacao_tran = sl_conhec-id_nomeacao_tran
        BINARY SEARCH
        TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DELETE t_conhec_a INDEX sy-tabix.
      ENDIF.
      CLEAR sl_conhec.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " Z_PREENCHE_CONTROLS

*&---------------------------------------------------------------------*
*&      Module  ZM_AVINC_MARC  INPUT                                   *
*&---------------------------------------------------------------------*
*                        Marcação a Vincular                           *
*----------------------------------------------------------------------*
MODULE zm_avinc_marc INPUT.

  MODIFY t_conhec_a FROM s_conhec_a INDEX tc_avinc-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_AVINC_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_VINC_MARC  INPUT                                    *
*&---------------------------------------------------------------------*
*                           Marcação Vinculado                         *
*----------------------------------------------------------------------*
MODULE zm_vinc_marc INPUT.

  MODIFY t_conhec_v FROM s_conhec_v INDEX tc_vinc-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_VINC_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_DESVINCULAR                                            *
*&---------------------------------------------------------------------*
*                            Desvincular                               *
*----------------------------------------------------------------------*
FORM z_desvincular.

  DATA: tl_conhec LIKE TABLE OF t_conhec_a,
        sl_conhec LIKE LINE OF  t_conhec_a,
        sl_full   LIKE LINE OF  t_conhec_a,
        vl_index  TYPE i.

  tl_conhec[] = t_conhec_v[].
  DELETE tl_conhec WHERE marc IS INITIAL.

  IF tl_conhec[] IS INITIAL.
    MESSAGE i836 WITH text-003.
    EXIT.
  ENDIF.

  LOOP AT t_conhec_v INTO sl_conhec WHERE marc NE space.
    vl_index = sy-tabix.
    READ TABLE t_full INTO sl_full
      WITH KEY id_conhec        = sl_conhec-id_conhec
               id_nomeacao_tran = sl_conhec-id_nomeacao_tran
      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      sl_full-id   = sl_conhec-id.
      sl_full-item = sl_conhec-item.
      APPEND sl_full TO t_conhec_a.
    ELSE.
      APPEND sl_conhec TO t_conhec_a.
    ENDIF.
    DELETE t_conhec_v INDEX vl_index.
    CLEAR sl_conhec.
  ENDLOOP.

ENDFORM.                    " Z_DESVINCULAR

*&---------------------------------------------------------------------*
*&      Form  Z_VINCULAR                                               *
*&---------------------------------------------------------------------*
*                                Vincular                              *
*----------------------------------------------------------------------*
FORM z_vincular.

  DATA: tl_conhec LIKE TABLE OF t_conhec_a,
        sl_conhec LIKE LINE OF  t_conhec_a,
        sl_conhev LIKE LINE OF  t_conhec_a,
        vl_index  TYPE i.

  tl_conhec[] = t_conhec_a[].
  DELETE tl_conhec WHERE marc IS INITIAL.

  IF tl_conhec[] IS INITIAL.
    MESSAGE i836 WITH text-004.
    EXIT.
  ENDIF.

  IF NOT t_conhec_v[] IS INITIAL.
    READ TABLE t_conhec_v[] INDEX 1 INTO sl_conhev.
  ENDIF.

  LOOP AT t_conhec_a INTO sl_conhec WHERE marc NE space.

    vl_index = sy-tabix.

    IF NOT t_conhec_v[] IS INITIAL.
      READ TABLE t_conhec_v[] INDEX 1 INTO sl_conhev.
      IF sl_conhev-sg_pais_destino EQ sl_conhec-sg_pais_destino.
        APPEND sl_conhec TO t_conhec_v.
        DELETE t_conhec_a INDEX vl_index.
        CLEAR sl_conhec.
      ENDIF.
    ELSE.
      APPEND sl_conhec TO t_conhec_v.
      DELETE t_conhec_a INDEX vl_index.
      CLEAR sl_conhec.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " Z_VINCULAR

*&---------------------------------------------------------------------*
*&      Form  Z_SAVE                                                   *
*&---------------------------------------------------------------------*
*                               Salvar                                 *
*----------------------------------------------------------------------*
FORM z_save.

  DATA vl_answer TYPE char1.

  IF t_vincu[] IS NOT INITIAL.
*  IF ( VG_MENGE_S GT 0 ) AND ( T_VINCU[] IS INITIAL ).
    IF vg_menge_s GT 0 .
      MESSAGE i836 WITH text-011.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text-005
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK vl_answer EQ '1'.

* Desvincular das Tabelas
  PERFORM: z_desvincular_tab,
* Vincular Tabelas
           z_vincular_tab   ,
* Vincula Notas do Produtor
           z_salva_notas_prod.

*   Preenche Table Controls
*  PERFORM z_preenche_controls.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CLEAR: s_cabec,
         v_vbeln.

  IF p_tcode IS NOT INITIAL.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " Z_SAVE

*&---------------------------------------------------------------------*
*&      Form  Z_DESVINCULAR_TAB                                        *
*&---------------------------------------------------------------------*
*                        Desvincular das Tabelas                       *
*----------------------------------------------------------------------*
FORM z_desvincular_tab.

  DATA: tl_conhec    LIKE TABLE OF t_conhec_a,
        sl_conhec    LIKE LINE OF  t_conhec_a,
        vl_id_doc    TYPE zid_doc,
        wl_reme_nota TYPE znom_reme_notas,
        wl_remet     TYPE znom_remetente.

  tl_conhec[] = t_conhec_a[].
  DELETE tl_conhec WHERE id EQ 0.

  CHECK NOT tl_conhec[] IS INITIAL.

  LOOP AT tl_conhec INTO sl_conhec.
    DELETE FROM zdoc_rem_bl
      WHERE id_doc_exp  EQ sl_conhec-id
        AND id_doc_item EQ sl_conhec-item.
    CLEAR sl_conhec.
  ENDLOOP.

  SORT tl_conhec BY id ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_conhec COMPARING id.

  MESSAGE i836 WITH text-006.

ENDFORM.                    " Z_DESVINCULAR_TAB

*&---------------------------------------------------------------------*
*&      Form  Z_VINCULAR_TAB                                           *
*&---------------------------------------------------------------------*
*                          Vincular Tabelas                            *
*----------------------------------------------------------------------*
FORM z_vincular_tab.

  DATA: tl_conhec  LIKE TABLE OF t_conhec_a,
        sl_conhec  LIKE LINE OF  t_conhec_a,
        sl_doc     TYPE zdoc_exp,
        sl_rem     TYPE zdoc_rem_bl,
        vl_id_item TYPE zid_item,
        vl_id_doc  TYPE zid_doc.

  CLEAR: vl_id_doc, vl_id_item.

  IF  p_tcode IS NOT INITIAL.

    IF s_cabec-id_due IS NOT INITIAL.
      SELECT SINGLE id_doc_exp
        FROM zdoc_exp INTO vl_id_doc
       WHERE vbeln    EQ v_vbeln
         AND id_due  EQ s_cabec-id_due.

    ELSE.
      SELECT SINGLE id_doc_exp
        FROM zdoc_exp INTO vl_id_doc
       WHERE vbeln    EQ v_vbeln
         AND id_due   EQ space.
    ENDIF.

  ELSE.

    IF ( v_vbeln     IS NOT INITIAL ) AND
       ( s_cabec-exp IS NOT INITIAL ).

      SELECT SINGLE id_doc_exp
        FROM zdoc_exp INTO vl_id_doc
       WHERE vbeln            EQ v_vbeln
         AND id_registro_expo EQ s_cabec-exp.

    ELSEIF ( v_vbeln  IS NOT INITIAL ) AND
           ( s_cabec-id_due IS NOT INITIAL ).

      SELECT SINGLE id_doc_exp
        FROM zdoc_exp INTO vl_id_doc
       WHERE vbeln   EQ v_vbeln
         AND id_due  EQ s_cabec-id_due.

    ELSE.
      RETURN.
    ENDIF.


  ENDIF.

  IF NOT sy-subrc IS INITIAL.

    SELECT MAX( id_doc_exp )
      INTO vl_id_doc
      FROM zdoc_exp.

    IF vl_id_doc EQ 9999999999.
      MESSAGE i836 WITH text-022.
      EXIT.
    ENDIF.

    IF vl_id_doc IS INITIAL.
      vl_id_doc = 1.
    ELSE.
      ADD 1 TO vl_id_doc.
    ENDIF.

    sl_doc-id_doc_exp       = vl_id_doc.
    sl_doc-vbeln            = v_vbeln.
    sl_doc-id_registro_expo = s_cabec-exp.
    sl_doc-nr_registro_expo = s_cabec-re.
    sl_doc-id_dde           = s_cabec-id_dde.
    sl_doc-nr_dde           = s_cabec-dde.
    sl_doc-id_nomeacao_tran = s_cabec-navio.
    sl_doc-id_due           = s_cabec-id_due.
    sl_doc-numero_due       = s_cabec-numero_due.

    DELETE FROM zdoc_exp WHERE vbeln EQ v_vbeln AND vbeln NE space.
    INSERT zdoc_exp FROM sl_doc.
    vl_id_item = 0.

  ELSE.

    SELECT MAX( id_doc_item )
      FROM zdoc_rem_bl
      INTO vl_id_item
    WHERE  id_doc_exp EQ vl_id_doc.

  ENDIF.

  tl_conhec[] = t_conhec_v[].
  DELETE tl_conhec WHERE id NE 0.

  CHECK NOT tl_conhec[] IS INITIAL.

  LOOP AT tl_conhec INTO sl_conhec.

    ADD 1 TO vl_id_item.
    sl_rem-id_doc_exp       = vl_id_doc.
    sl_rem-id_doc_item      = vl_id_item.
    sl_rem-id_conhec        = sl_conhec-id_conhec.
    sl_rem-id_nomeacao_tran = sl_conhec-id_nomeacao_tran.
    sl_rem-dt_data          = sl_conhec-dt_data.
    sl_rem-nr_qtde          = sl_conhec-nr_qtde.
    sl_rem-nr_conhec        = sl_conhec-nr_conhec.

    INSERT zdoc_rem_bl FROM sl_rem.

    CLEAR: sl_conhec,
           sl_doc   ,
           sl_rem   .

  ENDLOOP.

  MESSAGE i836 WITH text-007.

ENDFORM.                    " Z_VINCULAR_TAB

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CONTROLS_NF_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_preenche_controls_nf_prod .

  IF s_prod-werks IS INITIAL.
    MESSAGE text-009 TYPE 'S' DISPLAY LIKE 'E'.
  ELSEIF j_1bnfdoc-docdat IS INITIAL OR j_1bnfdoc-pstdat IS INITIAL.
    MESSAGE text-010 TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM seleciona_notas.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_CONTROLS_NF_PROD

*&---------------------------------------------------------------------*
*&      Module  ZM_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_werks INPUT.

  SELECT SINGLE * INTO s_prod
    FROM t001w
   WHERE werks EQ s_prod-werks.

ENDMODULE.                 " ZM_WERKS  INPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_NOTAS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_notas_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_notas LINES tab_notas-lines.
ENDMODULE.                    "TAB_NOTAS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_NOTAS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tab_notas_mark INPUT.
  DATA: g_tab_notas_wa2 LIKE LINE OF t_notas.
  IF tab_notas-line_sel_mode = 1
  AND t_notas-mark = 'X'.
    LOOP AT t_notas INTO g_tab_notas_wa2
      WHERE mark = 'X'.
      g_tab_notas_wa2-mark = ''.
      MODIFY t_notas
        FROM g_tab_notas_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY t_notas
    INDEX tab_notas-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_NOTAS_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_NOTAS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tab_notas_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TAB_NOTAS'
                              'T_NOTAS'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TAB_NOTAS_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_VINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_vinc_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_vincu LINES tab_vinc-lines.
ENDMODULE.                    "TAB_VINC_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_VINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tab_vinc_mark INPUT.
  DATA: g_tab_vinc_wa2 LIKE LINE OF t_vincu.
  IF tab_vinc-line_sel_mode = 1
  AND t_vincu-mark = 'X'.
    LOOP AT t_vincu INTO g_tab_vinc_wa2
      WHERE mark = 'X'.
      g_tab_vinc_wa2-mark = ''.
      MODIFY t_vincu
        FROM g_tab_vinc_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY t_vincu
    INDEX tab_vinc-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_VINC_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_VINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tab_vinc_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TAB_VINC'
                              'T_VINCU'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TAB_VINC_USER_COMMAND INPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vincular  USING  p_vg_automatico.

*  vg_menge   type j_1bnetqty, "Quantidade da Nota Fiscal de Exportação
*  vg_menge_s type j_1bnetqty, "Quantidade Saldo a Vincular
*  vg_menge_v type j_1bnetqty, "Quantidade vinculada
*  vg_menge_l type j_1bnetqty. "Quantidade livre para vincular

  DATA: vg_menge_aux    TYPE j_1bnetqty,
        vg_menge_nf     TYPE j_1bnetqty,
        vg_menge_saldo  TYPE j_1bnetqty,
        vg_qtd_vinc_now TYPE j_1bnetqty,
        vg_tabix        TYPE sy-tabix,
        vg_qtd_sel      TYPE i.

  PERFORM busca_valores.

*  IF vg_menge_qp GT 0.
*    IF p_vg_automatico EQ 'X'.
*      MESSAGE e836 WITH text-012.
*    ENDIF.
*    vg_qtd_sel = 0.
*    LOOP AT t_notas INTO w_notas WHERE mark EQ 'X'.
*      vg_qtd_sel = vg_qtd_sel + 1.
*    ENDLOOP.
*    IF vg_qtd_sel NE 1.
*      MESSAGE e836 WITH text-013.
*    ENDIF.
*  ENDIF.

  vg_qtd_vinc_now = 0.

  LOOP AT t_notas INTO w_notas.

    vg_tabix = sy-tabix.

    IF w_notas-mark EQ 'X' OR p_vg_automatico EQ 'X'.
*      IF vg_menge_qp GT 0.
*        IF vg_menge_qp GT w_notas-saldo.
*          MESSAGE e836 WITH text-014.
*        ENDIF.
*        vg_menge_saldo = vg_menge_qp.
*      ELSE.
*        vg_menge_saldo = w_notas-saldo.
*      ENDIF.

      vg_menge_saldo = w_notas-saldo.

      vg_menge_nf = vg_menge_saldo.

      IF vg_menge_saldo LE vg_menge_s.
        vg_menge_s = vg_menge_s - vg_menge_saldo.
      ELSEIF vg_menge_s GT 0.
        vg_menge_saldo = vg_menge_s.
        vg_menge_s     = 0.
      ELSE.
        vg_menge_saldo = 0.
      ENDIF.

      IF ( vg_menge_qp GT 0 ) AND ( vg_menge_saldo GT 0 ).
        IF vg_qtd_vinc_now GE vg_menge_qp.
          vg_menge_saldo = 0.
        ELSEIF vg_qtd_vinc_now LT vg_menge_qp.
          vg_menge_aux   = vg_qtd_vinc_now + vg_menge_saldo.
          IF vg_menge_aux GT vg_menge_qp.
            vg_menge_saldo = vg_menge_qp - vg_qtd_vinc_now.
          ENDIF.
        ENDIF.
      ENDIF.

      IF vg_menge_saldo GT 0.
        CLEAR: w_vincu.
        vg_menge_v = vg_menge_v + vg_menge_saldo.
        vg_menge_l = vg_menge_l - vg_menge_saldo.

        READ TABLE t_vincu INTO w_vincu WITH KEY docnum_prod = w_notas-docnum itmnum_prod = w_notas-itmnum.
        IF sy-subrc EQ 0.
          w_vincu-menge = w_vincu-menge + vg_menge_saldo.
          MODIFY t_vincu INDEX sy-tabix FROM w_vincu TRANSPORTING menge.
        ELSE.
          w_vincu-vbeln       = v_vbeln.
          w_vincu-docnum_prod = w_notas-docnum.
          w_vincu-itmnum_prod = w_notas-itmnum.
          w_vincu-menge       = vg_menge_saldo.
          w_vincu-docdat      = w_notas-docdat.
          w_vincu-nfenum      = w_notas-nfenum.
          w_vincu-charg       = w_notas-charg.
          w_vincu-nome        = w_notas-nome.
          w_vincu-uf          = w_notas-uf.
          w_vincu-parid       = w_notas-parid.
          w_vincu-meins       = w_notas-meins.
          w_vincu-fiscal      = w_notas-fiscal.
          APPEND w_vincu TO t_vincu.
        ENDIF.
        w_notas-saldo   = vg_menge_nf - vg_menge_saldo.
        vg_qtd_vinc_now = vg_qtd_vinc_now + vg_menge_saldo.
        MODIFY t_notas INDEX vg_tabix FROM w_notas TRANSPORTING saldo.
        DELETE t_desvi WHERE docnum_prod EQ w_notas-docnum AND itmnum_prod EQ w_notas-itmnum.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT t_vincu BY docnum_prod itmnum_prod.

  IF vg_menge_qp GT 0.
    IF vg_qtd_vinc_now LT vg_menge_qp.
      vg_menge_qp = vg_menge_qp - vg_qtd_vinc_now.
    ELSE.
      vg_menge_qp = 0.
    ENDIF.
  ENDIF.

  PERFORM busca_valores.

ENDFORM.                    " VINCULAR

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_valores .

  DATA: vg_refkey TYPE j_1brefkey,
        vg_posnr  TYPE posnr_vf,
        vg_cancel TYPE cancel.

  vg_menge   = 0.
  vg_menge_s = 0.
  vg_menge_v = 0.
  vg_menge_l = 0.

  "Busca Documento de Remessa.
  SELECT SINGLE ntgew
    INTO vg_menge
    FROM lips
   WHERE vbeln EQ v_vbeln.

  LOOP AT t_notas INTO w_notas.
    vg_menge_l = vg_menge_l + w_notas-saldo.
  ENDLOOP.

  LOOP AT t_vincu INTO w_vincu.
    vg_menge_v = vg_menge_v + w_vincu-menge.
  ENDLOOP.

  vg_menge_s = vg_menge - vg_menge_v.

  PERFORM agrupa_produtores_volume.

*  vg_menge   type j_1bnetqty, "Quantidade da Nota Fiscal de Exportação
*  vg_menge_s type j_1bnetqty, "Quantidade Saldo a Vincular
*  vg_menge_v type j_1bnetqty, "Quantidade vinculada
*  vg_menge_l type j_1bnetqty. "Quantidade livre para vincular

ENDFORM.                    " BUSCA_VALORES

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desvincular .
*  REFRESH T_DESVI.

  LOOP AT t_vincu INTO w_vincu WHERE mark EQ 'X'.
    CLEAR w_vincu-mark.
    APPEND w_vincu TO t_desvi.
  ENDLOOP.

  DELETE t_vincu WHERE mark EQ 'X'.
  PERFORM seleciona_notas.

ENDFORM.                    " DESVINCULAR

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_notas .

  TYPES: BEGIN OF zmaterial,
           sign      TYPE c,
           option(2) TYPE c,
           low       TYPE c LENGTH 18,
           high      TYPE c LENGTH 18,
         END OF zmaterial.

  TYPES: BEGIN OF zprodutor,
           sign      TYPE c,
           option(2) TYPE c,
           low       TYPE c LENGTH 10,
           high      TYPE c LENGTH 10,
         END OF zprodutor.

  DATA: it_zsdt0024 TYPE TABLE OF zsdt0024 INITIAL SIZE 0 WITH HEADER LINE,
        wa_zsdt0024 TYPE zsdt0024.

  DATA: vl_id_doc    TYPE zid_doc,
        vg_vbeln     TYPE vbeln_vf,
        vg_refkey    TYPE j_1brefkey,
        vg_posnr     TYPE posnr_vf,
        vg_cancel    TYPE cancel,
        dt_inicio    LIKE j_1bnfdoc-docdat,
        dt_final     LIKE j_1bnfdoc-docdat,
        cfops        TYPE TABLE OF lxhme_range_c10,
        wa_info_part TYPE lfa1,
        wa_produtor  TYPE zprodutor,
        it_produtor  TYPE TABLE OF zprodutor,
        wa_materiais TYPE zmaterial,
        it_materiais TYPE TABLE OF zmaterial,
        vg_data_60   TYPE sy-datum,
        vg_data_90   TYPE sy-datum,
        vg_tabix     TYPE sy-tabix.

  CLEAR: t_notas[], t_notas_aux[], t_vincu_aux[], t_reme_nota[], t_remetente[].

  PERFORM busca_valores.

  dt_inicio  = j_1bnfdoc-docdat.
  dt_final   = j_1bnfdoc-pstdat.

  "Busca Documento de Remessa.
  SELECT SINGLE matnr ntgew
    INTO (vg_matnr, vg_menge)
    FROM lips
   WHERE vbeln EQ v_vbeln.

  CHECK NOT vg_matnr IS INITIAL.

  SELECT SINGLE matkl
    FROM mara
    INTO vg_matkl
    WHERE matnr EQ vg_matnr.

  SELECT * INTO TABLE it_zsdt0024
    FROM zsdt0024
   WHERE matnr1 EQ vg_matnr.

  wa_materiais-sign   = 'I'.
  wa_materiais-option = 'EQ'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vg_matnr
    IMPORTING
      output = vg_matnr.

  wa_materiais-low    = conv #( vg_matnr ).
  wa_materiais-high   = conv #( vg_matnr ).
  APPEND wa_materiais TO it_materiais.

  LOOP AT it_zsdt0024 INTO wa_zsdt0024.
    wa_materiais-sign   = 'I'.
    wa_materiais-option = 'EQ'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0024-matnr2
      IMPORTING
        output = wa_zsdt0024-matnr2.

    wa_materiais-low    = conv #( wa_zsdt0024-matnr2 ).
    wa_materiais-high   = conv #( wa_zsdt0024-matnr2 ).
    APPEND wa_materiais TO it_materiais.
  ENDLOOP.

  IF NOT t_vincu[] IS INITIAL.
    MOVE t_vincu[] TO t_vincu_aux[].
    CLEAR: t_vincu[].
  ENDIF.

  SELECT vc~vbeln vc~docnum_prod vc~itmnum_prod vc~menge
         dc~docdat dc~nfenum li~charg dc~parid dc~partyp li~meins
    INTO CORRESPONDING FIELDS OF TABLE t_vincu
    FROM zdoc_nf_produtor AS vc
   INNER JOIN j_1bnfdoc AS dc ON dc~docnum EQ vc~docnum_prod
   INNER JOIN j_1bnflin AS li ON li~docnum EQ vc~docnum_prod AND li~itmnum EQ vc~itmnum_prod
   WHERE vc~vbeln EQ v_vbeln.

  LOOP AT t_desvi INTO w_vincu.
    DELETE t_vincu WHERE docnum_prod EQ w_vincu-docnum_prod AND itmnum_prod EQ w_vincu-itmnum_prod.
  ENDLOOP.

  LOOP AT t_vincu_aux INTO w_vincu.
    DELETE t_vincu WHERE docnum_prod EQ w_vincu-docnum_prod AND itmnum_prod EQ w_vincu-itmnum_prod.
    APPEND w_vincu TO t_vincu.
  ENDLOOP.

  CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
    TABLES
      cfops = cfops.

  IF NOT s_produtor-lifnr IS INITIAL.
    wa_produtor-sign   = 'I'.
    wa_produtor-option = 'EQ'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_produtor-lifnr
      IMPORTING
        output = s_produtor-lifnr.

    wa_produtor-low    = s_produtor-lifnr.
    wa_produtor-high   = s_produtor-lifnr.
    APPEND wa_produtor TO it_produtor.
  ENDIF.


  IF ok_code EQ 'BTPSQN'.
    IF dt_inicio IS NOT INITIAL AND dt_final IS NOT INITIAL.

      IF p_tcode EQ 'ZSDT0066'.
        FREE it_materiais.
      ENDIF.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE t_notas_aux
        FROM j_1bnfdoc AS dc
       INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
       WHERE dc~bukrs  EQ s_cabec-bukrs
         AND dc~branch EQ s_prod-werks
         AND dc~direct EQ '1'
         AND dc~cancel EQ vg_cancel
         AND dc~doctyp NE '5'
         AND dc~parid  IN it_produtor
         AND dc~docdat GE dt_inicio
         AND dc~docdat LE dt_final
         AND li~cfop   IN cfops
         AND li~matnr  IN it_materiais   "#EC CI_FLDEXT_OK[2215424]
         AND li~matkl  EQ vg_matkl.

    ENDIF.
  ELSE.

    DATA: it_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t.
    DATA: r_docnum TYPE RANGE OF j_1bdocnum.
    DATA: r_doc TYPE RANGE OF j_1bdocnum.

    SELECT docnum
      FROM zsdt_retlote
       INTO TABLE @DATA(it_docnum)
      WHERE vbeln EQ @v_vbeln.

    IF sy-subrc IS NOT INITIAL.

      SELECT docnum_rt
        FROM zsdt0053
        INTO TABLE @DATA(it_0053)
        WHERE remessa_exp EQ @v_vbeln.

      IF sy-subrc IS INITIAL.

        SELECT docnum
          FROM zsdt_retlote
          INTO TABLE @it_docnum
          FOR ALL ENTRIES IN @it_0053
          WHERE docnum_ret EQ @it_0053-docnum_rt.

      ENDIF.

    ENDIF.

    LOOP AT it_docnum INTO DATA(w_doc).

      CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
        EXPORTING
          i_docnum           = w_doc-docnum
        IMPORTING
          e_zsdt0001_ro_vinc = it_zsdt0001_ro_vinc.

      r_doc = VALUE #(
      FOR ls IN it_zsdt0001_ro_vinc
      LET s = 'I'
          o = 'EQ'
         IN sign = s
           option = o
        ( low = ls-docnum_vinc ) ).

      APPEND LINES OF r_doc TO r_docnum.

    ENDLOOP.

    SORT r_docnum BY low.
    DELETE ADJACENT DUPLICATES FROM r_docnum COMPARING low.

    IF r_docnum IS NOT INITIAL.
      SELECT *
         INTO CORRESPONDING FIELDS OF TABLE t_notas_aux
         FROM j_1bnfdoc AS dc
        INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
        WHERE dc~docnum IN r_docnum.
    ENDIF.
  ENDIF.

  LOOP AT t_notas_aux INTO w_notas.

    vg_tabix = sy-tabix.

    IF w_notas-meins NE 'KG'.

      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = w_notas-matnr
          i_mein1             = w_notas-meins
          i_meins             = 'KG'
          i_menge             = w_notas-menge
        IMPORTING
          menge               = w_notas-menge
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        w_notas-meins = 'KG'.
        MODIFY t_notas_aux INDEX vg_tabix FROM w_notas TRANSPORTING menge meins.
      ENDIF.

    ENDIF.

    SELECT SUM( menge ) INTO w_notas-saldo
      FROM zdoc_nf_produtor
     WHERE docnum_prod EQ w_notas-docnum
       AND itmnum_prod EQ w_notas-itmnum
       AND vbeln       NE v_vbeln.

    IF ( w_notas-nfe IS INITIAL ) AND ( NOT w_notas-nfnum IS INITIAL ).
      WRITE w_notas-nfnum TO w_notas-nfenum.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_notas-nfenum
        IMPORTING
          output = w_notas-nfenum.
    ENDIF.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = w_notas-parid
        p_partype    = w_notas-partyp
      CHANGING
        wa_info_part = wa_info_part.

    w_notas-nome  = wa_info_part-name1.
    w_notas-uf    = wa_info_part-regio.
    IF wa_info_part-stkzn IS INITIAL.
      WRITE wa_info_part-stcd1 USING EDIT MASK '__.___.___/____-__' TO w_notas-fiscal.
    ELSE.
      WRITE wa_info_part-stcd2 USING EDIT MASK '___.___.___-__' TO w_notas-fiscal.
    ENDIF.

    IF w_notas-saldo IS INITIAL.
      w_notas-saldo = 0.
    ENDIF.

    IF w_notas-menge GT w_notas-saldo.
      w_notas-saldo   = w_notas-menge - w_notas-saldo.
      READ TABLE t_vincu INTO w_vincu WITH KEY docnum_prod = w_notas-docnum itmnum_prod = w_notas-itmnum.
      IF sy-subrc EQ 0.
        w_notas-saldo = w_notas-saldo - w_vincu-menge.
      ENDIF.
      w_notas-vbeln  = v_vbeln.
      APPEND w_notas TO t_notas.
    ENDIF.

    IF w_notas-saldo GT 0.
      vg_data_60 = sy-datum - 60.
      vg_data_90 = sy-datum - 90.
      IF ( vg_data_60 LT w_notas-docdat ) .
        w_notas-status = icon_status_ok.
      ELSEIF ( vg_data_60 GE w_notas-docdat ).
        IF ( vg_data_90 LT w_notas-docdat ).
          w_notas-status = icon_status_alert.
        ELSE.
          w_notas-status = icon_status_critical.
        ENDIF.
      ENDIF.
    ELSE.
      w_notas-status = icon_status_best.
    ENDIF.

  ENDLOOP.

  LOOP AT t_vincu INTO w_vincu.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = w_vincu-parid
        p_partype    = w_vincu-partyp
      CHANGING
        wa_info_part = wa_info_part.

    w_vincu-nome  = wa_info_part-name1.
    w_vincu-uf    = wa_info_part-regio.

    IF wa_info_part-stkzn IS INITIAL.
      WRITE wa_info_part-stcd1 USING EDIT MASK '__.___.___/____-__' TO w_vincu-fiscal.
    ELSE.
      WRITE wa_info_part-stcd2 USING EDIT MASK '___.___.___-__' TO w_vincu-fiscal.
    ENDIF.

    MODIFY t_vincu INDEX sy-tabix FROM w_vincu TRANSPORTING nome fiscal uf.

  ENDLOOP.

  SORT t_notas BY docdat docnum itmnum.
  SORT t_vincu BY docdat docnum_prod itmnum_prod.

  PERFORM busca_valores.

ENDFORM.                    " SELECIONA_NOTAS

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_NOTAS_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_salva_notas_prod .

  DATA: lv_mat TYPE j_1bnflin-matnr.

  DELETE FROM zdoc_nf_produtor WHERE vbeln EQ v_vbeln.

  LOOP AT t_vincu INTO w_vincu.

    MOVE-CORRESPONDING w_vincu TO wnfprodutor.
    wnfprodutor-id_nomeacao_tran =  s_cabec-navio.

    MODIFY zdoc_nf_produtor FROM wnfprodutor.

    CLEAR: wl_rem_not.

    wl_rem_not-id_due           = s_cabec-id_due.
    wl_rem_not-numero_due       = s_cabec-numero_due.
    wl_rem_not-id_nomeacao_tran = s_cabec-navio.
    wl_rem_not-id_empresa       = s_cabec-bukrs.
    wl_rem_not-id_filial        = s_prod-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_cabec-cd_material
      IMPORTING
        output = wl_rem_not-id_material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_vincu-parid
      IMPORTING
        output = wl_rem_not-id_remetente.

    wl_rem_not-docnum        = w_vincu-docnum_prod.
    wl_rem_not-itmnum        = w_vincu-itmnum_prod.
    wl_rem_not-id_unidade    = w_vincu-meins.

    SELECT * FROM zdoc_nf_produtor
      INTO TABLE tw_nf_produtor
      WHERE docnum_prod EQ w_vincu-docnum_prod AND
            itmnum_prod EQ w_vincu-itmnum_prod.

    IF NOT tw_nf_produtor[] IS INITIAL.

      SELECT * FROM zdoc_exp
        INTO TABLE tw_exp
        FOR ALL ENTRIES IN tw_nf_produtor
        WHERE vbeln EQ tw_nf_produtor-vbeln
        AND id_nomeacao_tran EQ s_cabec-navio.

      LOOP AT tw_nf_produtor.
        READ TABLE tw_exp TRANSPORTING NO FIELDS WITH KEY vbeln = tw_nf_produtor-vbeln
                                               id_nomeacao_tran = s_cabec-navio.
        IF sy-subrc IS INITIAL.
          wl_rem_not-nr_quantidade = wl_rem_not-nr_quantidade + tw_nf_produtor-menge.
        ENDIF.
      ENDLOOP.

    ENDIF.

    MODIFY znom_reme_notas FROM wl_rem_not.

* Inclui na Tabela para não duplicar na Zmemo
    PERFORM z_prog_reme USING wl_rem_not.

  ENDLOOP.

  LOOP AT t_grupo INTO w_grupo.
    CLEAR: wl_reme.
    wl_reme-id_nomeacao_tran  = s_cabec-navio.
    wl_reme-id_empresa        = s_cabec-bukrs.
    wl_reme-id_filial         = s_prod-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_cabec-cd_material
      IMPORTING
        output = lv_mat.

    SELECT SUM( vc~menge )
      INTO wl_reme-nr_programada
      FROM zdoc_nf_produtor AS vc
     INNER JOIN j_1bnfdoc AS dc ON dc~docnum EQ vc~docnum_prod
     INNER JOIN j_1bnflin AS li ON li~docnum EQ vc~docnum_prod AND li~itmnum EQ vc~itmnum_prod
     WHERE dc~parid = w_grupo-parid
       AND li~matnr = lv_mat.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_cabec-cd_material
      IMPORTING
        output = wl_reme-id_material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_grupo-parid
      IMPORTING
        output = wl_reme-id_remetente.

    wl_reme-id_unidade     = w_grupo-meins.

    MODIFY znom_remetente FROM wl_reme.
  ENDLOOP.


  LOOP AT t_desvi INTO w_vincu.

    CLEAR: wl_rem_not.
    wl_rem_not-id_nomeacao_tran = s_cabec-navio.
    wl_rem_not-id_empresa       = s_cabec-bukrs.
    wl_rem_not-id_filial        = s_prod-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_cabec-cd_material
      IMPORTING
        output = wl_rem_not-id_material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_vincu-parid
      IMPORTING
        output = wl_rem_not-id_remetente.

    wl_rem_not-docnum        = w_vincu-docnum_prod.
    wl_rem_not-itmnum        = w_vincu-itmnum_prod.
    wl_rem_not-id_unidade    = w_vincu-meins.


    SELECT * FROM zdoc_nf_produtor
          INTO TABLE tw_nf_produtor
          WHERE docnum_prod EQ w_vincu-docnum_prod AND
                itmnum_prod EQ w_vincu-itmnum_prod.

    IF NOT tw_nf_produtor[] IS INITIAL.

      SELECT * FROM zdoc_exp
        INTO TABLE tw_exp
        FOR ALL ENTRIES IN tw_nf_produtor
        WHERE vbeln EQ tw_nf_produtor-vbeln
        AND id_nomeacao_tran EQ s_cabec-navio.

      LOOP AT tw_nf_produtor.
        READ TABLE tw_exp TRANSPORTING NO FIELDS WITH KEY vbeln = tw_nf_produtor-vbeln
                                               id_nomeacao_tran = s_cabec-navio.
        IF sy-subrc IS INITIAL.
          wl_rem_not-nr_quantidade = wl_rem_not-nr_quantidade + tw_nf_produtor-menge.
        ENDIF.
      ENDLOOP.

    ENDIF.

*    WL_REM_NOT-NR_QUANTIDADE = WL_REM_NOT-NR_QUANTIDADE - W_VINCU-MENGE.

    IF wl_rem_not-nr_quantidade LE 0.

      DELETE FROM znom_reme_notas WHERE id_remetente     EQ w_vincu-parid
                                    AND id_nomeacao_tran EQ s_cabec-navio
                                    AND docnum           EQ w_vincu-docnum_prod
                                    AND itmnum           EQ w_vincu-itmnum_prod.

    ELSE.
      MODIFY znom_reme_notas FROM wl_rem_not.
    ENDIF.

  ENDLOOP.


  CLEAR: t_vincu[], t_notas[], s_prod, j_1bnfdoc, t_grupo[], t_desvi[].

  vg_menge   = 0.
  vg_menge_s = 0.
  vg_menge_v = 0.
  vg_menge_l = 0.

ENDFORM.                    " Z_SALVA_NOTAS_PROD

*&---------------------------------------------------------------------*
*&      Form  AGRUPA_PRODUTORES_VOLUME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM agrupa_produtores_volume .

*  DATA: vg_debug(1) TYPE c.

  CLEAR: t_vincu_aux[], t_grupo[], t_remetente[].

  MOVE t_vincu[] TO t_vincu_aux[].

  SORT: t_vincu_aux[] BY uf parid.

*  IF vg_debug IS NOT INITIAL.
*
*    LOOP AT t_vincu_aux INTO w_vincu.
*
*      AT NEW parid.
*        CLEAR: w_grupo.
*        w_grupo-parid  = w_vincu-parid.
*        w_grupo-volume = 0.
*      ENDAT.
*
*      w_grupo-nome   = w_vincu-nome.
*      w_grupo-uf     = w_vincu-uf.
*      w_grupo-fiscal = w_vincu-fiscal.
*      w_grupo-volume = w_grupo-volume + w_vincu-menge.
*
*      AT END OF parid.
*        APPEND w_grupo TO t_grupo.
*      ENDAT.
*
*    ENDLOOP.
*
*  ELSE.

  CLEAR: w_grupo.

  LOOP AT t_vincu_aux INTO w_vincu.

    w_grupo-parid  = w_vincu-parid.

    ON CHANGE OF w_vincu-parid.
      IF w_grupo IS NOT INITIAL.
        APPEND w_grupo TO t_grupo.
      ENDIF.
      CLEAR: w_grupo.
      w_grupo-parid  = w_vincu-parid.
      w_grupo-volume = 0.
    ENDON.

    w_grupo-meins  = w_vincu-meins.
    w_grupo-nome   = w_vincu-nome.
    w_grupo-uf     = w_vincu-uf.
    w_grupo-fiscal = w_vincu-fiscal.
    w_grupo-volume = w_grupo-volume + w_vincu-menge.
  ENDLOOP.

  IF w_grupo IS NOT INITIAL.
    APPEND w_grupo TO t_grupo.
  ENDIF.
*  ENDIF.

  SORT t_grupo BY parid uf.

ENDFORM.                    " AGRUPA_PRODUTORES_VOLUME

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE sy-ucomm.
    WHEN tabs-tab1.
      vg_dynnr = '0003'.
      vg_main100_tabstrip-activetab = tabs-tab1.
    WHEN tabs-tab2.
      vg_dynnr = '0004'.
      vg_main100_tabstrip-activetab = tabs-tab2.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*----------------------------------------------------------------------*
*  MODULE TAB_VINC_GRUPO_CHANGE_TC_ATTR OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE tab_vinc_grupo_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_grupo LINES tab_vinc_grupo-lines.
ENDMODULE.                    "TAB_VINC_GRUPO_CHANGE_TC_ATTR OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_FORNECEDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_fornecedor INPUT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = s_produtor-lifnr
    IMPORTING
      output = s_produtor-lifnr.

  SELECT SINGLE * INTO s_produtor
    FROM lfa1
   WHERE lifnr EQ s_produtor-lifnr.

ENDMODULE.                 " ZM_FORNECEDOR  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_DESVINCULA_NF_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_desvincula_nf_prod .

  DATA: wa_vbrp       TYPE vbrp,
        wa_j_1bnflin  TYPE j_1bnflin,
        vg_j_1brefkey TYPE j_1brefkey,
        wa_memo       TYPE zdoc_memo_nf_exp,
        vl_answer     TYPE char1.

  SELECT SINGLE * INTO wa_vbrp
    FROM vbrp
   WHERE vgbel EQ v_vbeln AND DRAFT = SPACE .

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-001.
  ENDIF.

  vg_j_1brefkey = wa_vbrp-vbeln.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE refkey EQ vg_j_1brefkey.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_memo
      FROM zdoc_memo_nf_exp
     WHERE docnum EQ wa_j_1bnflin-docnum.

    IF sy-subrc IS INITIAL.
      MESSAGE e836 WITH text-015.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text-016
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK vl_answer EQ '1'.

  DELETE FROM zdoc_nf_produtor WHERE vbeln EQ v_vbeln.
  COMMIT WORK.

  MESSAGE s836 WITH text-017.

ENDFORM.                    " Z_DESVINCULA_NF_PROD
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_MATERIAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_valida_materiais .
***  Ch.127923 - Marcos Faneli
  DATA: BEGIN OF tl_exportacao OCCURS 0,
          cd_material TYPE zreg_exportacao-cd_material,
        END OF tl_exportacao,

        BEGIN OF tl_lips OCCURS 0,
          matnr TYPE lips-matnr,
        END OF tl_lips.

  DATA: BEGIN OF tl_0093 OCCURS 0,
          matnr TYPE mara-matnr,
        END OF tl_0093.

  DATA: lv_cont     TYPE i,
        lv_material TYPE zreg_exportacao-cd_material.

  CLEAR: lv_cont.

* Encontra os materiais da remessa
  SELECT *
    FROM lips
    INTO CORRESPONDING FIELDS OF TABLE tl_lips
    WHERE vbeln EQ v_vbeln.

* Encontra os materiais do RE
  SELECT *
    FROM zreg_exportacao
    INTO CORRESPONDING FIELDS OF TABLE tl_exportacao
    WHERE nr_registro_expo EQ s_cabec-re.

* Verifica se a remessa e RE possuem os mesmo materiais
  LOOP AT tl_lips.
    SHIFT tl_lips-matnr LEFT DELETING LEADING '0'.

    READ TABLE tl_exportacao WITH KEY cd_material = tl_lips-matnr.

    IF sy-subrc IS INITIAL.
      ADD 1 TO lv_cont.
    ENDIF.
  ENDLOOP.

  IF lv_cont IS INITIAL.
*   Verifica se os materiais RE tem vinculo na tabela de De/Para zsdt0093
    SELECT *
      FROM zsdt0093
      JOIN mara ON mara~matkl EQ zsdt0093~matkl
      INTO CORRESPONDING FIELDS OF TABLE tl_0093
      FOR ALL ENTRIES IN tl_exportacao
      WHERE matnr_com EQ tl_exportacao-cd_material.

    IF tl_0093[] IS NOT INITIAL.
      LOOP AT tl_lips.
        READ TABLE tl_0093 WITH KEY matnr = tl_lips-matnr.
        IF sy-subrc IS INITIAL.
          ADD 1 TO lv_cont.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Interrompe o processo caso não haja vinculo entre os materiais
    IF lv_cont IS INITIAL.
      MESSAGE text-018 TYPE 'E'.
      CLEAR: v_vbeln, s_cabec-re.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VALIDA_MATERIAIS
*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_preenche_re .

*  Buscar RE/DU-e vinculada a Rmessa informada.
*  SELECT SINGLE NR_REGISTRO_EXPO NUMERO_DUE
*    FROM ZDOC_EXP INTO (WA_VC_DOC_EXP-NR_REGISTRO_EXPO, WA_VC_DOC_EXP-NUMERO_DUE)
*   WHERE VBELN = WA_VC_DOC_EXP-VBELN.

*  IF ( SY-SUBRC IS INITIAL ) AND ( WA_VC_DOC_EXP-VBELN IS NOT INITIAL ).
*    PERFORM Z_VERIFICA_RE.
*  PERFORM Z_VERIFICA_DUE.
*  ENDIF.

*  Buscar RE vinculada a Rmessa informada.
  SELECT SINGLE nr_registro_expo numero_due
    FROM zdoc_exp
    INTO ( s_cabec-re, s_cabec-numero_due )
    WHERE vbeln = v_vbeln.

  IF ( sy-subrc IS INITIAL ) AND ( v_vbeln IS NOT INITIAL ).
    PERFORM z_verifica_re.
    PERFORM z_verifica_due.
  ENDIF.
ENDFORM.                    " Z_PREENCHE_RE
*&---------------------------------------------------------------------*
*&      Form  Z_VERFICA_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_verifica_re .
  REFRESH: t_conhec_a,
           t_conhec_v,
           t_reme_nota,
           t_remetente,
           t_full    .

*  CLEAR: S_CABEC.

  CHECK NOT s_cabec-re IS INITIAL.

  SELECT SINGLE nr_registro_expo
    FROM zreg_exportacao
    INTO s_cabec-re
   WHERE nr_registro_expo EQ s_cabec-re
     AND in_status_comex  NE 'X'.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-002.
  ELSE.
    PERFORM z_valida_materiais.
*   Preenche Dados DDE/Navio
    PERFORM z_preenche_dde.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
  ENDIF.
ENDFORM.                    " Z_VERFICA_RE
*&---------------------------------------------------------------------*
*&      Module  MODIFICA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modifica_tela OUTPUT.

  "IMPORT V_TCODE  FROM MEMORY ID 'P_TCODE'.

*  IF P_TCODE IS NOT INITIAL.
*    P_TCODE = V_TCODE.
*    DELETE FROM MEMORY ID 'P_TCODE'.
*  ENDIF.

  LOOP AT SCREEN.

    IF screen-name EQ 'V_VBELN' AND p_tcode IS NOT INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name EQ 'S_CABEC-RE' OR screen-name EQ 'S_CABEC-NUMERO_DUE'.
      IF v_vbeln IS INITIAL.
        screen-input = 0.
      ELSEIF s_cabec-re IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
*   O Botão DESVINCULAR fica visivel somente quando vem da ZSDT0066
*    IF S_CABEC-ID_DUE IS INITIAL OR S_CABEC-NUMERO_DUE IS INITIAL.
    IF screen-name EQ 'DESVINCULAR' AND p_tcode NE 'ZSDT0066'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
*    ENDIF.
  ENDLOOP.

ENDMODULE.                 " MODIFICA_TELA  OUTPUT


FORM z_prog_reme USING  wl_rem_not LIKE znom_reme_notas.

  MOVE: wl_rem_not-id_nomeacao_tran TO tw_prog_reme-id_nomeacao_tran,
        wl_rem_not-id_empresa       TO tw_prog_reme-id_empresa,
        wl_rem_not-id_filial        TO tw_prog_reme-id_filial,
        wl_rem_not-id_material      TO tw_prog_reme-id_material,
        v_vbeln                     TO tw_prog_reme-id_remessa,
        s_cabec-id_due              TO tw_prog_reme-id_due,
        s_cabec-exp                 TO tw_prog_reme-id_registro_expo.

  IF tw_prog_reme-id_remessa IS INITIAL.
    MODIFY znom_prog_reme FROM tw_prog_reme.
  ELSE.

    SELECT *
      FROM znom_prog_reme
      INTO TABLE @DATA(it_znom_prog_reme)
      WHERE id_remessa EQ @tw_prog_reme-id_remessa.

    IF sy-subrc IS INITIAL.

      IF lines( it_znom_prog_reme ) NE 1.
        DELETE FROM znom_prog_reme WHERE id_remessa EQ tw_prog_reme-id_remessa
                                     AND id_nomeacao_tran EQ '0000000000'.
      ENDIF.

      UPDATE znom_prog_reme
        SET id_nomeacao_tran = tw_prog_reme-id_nomeacao_tran
            id_empresa       = tw_prog_reme-id_empresa
            id_filial        = tw_prog_reme-id_filial
            id_material      = tw_prog_reme-id_material
            id_registro_expo = tw_prog_reme-id_registro_expo
            id_due           = tw_prog_reme-id_due
        WHERE id_remessa EQ tw_prog_reme-id_remessa.

    ELSE.
      MODIFY znom_prog_reme FROM tw_prog_reme.
    ENDIF.

  ENDIF.

ENDFORM.                    " Z_PROG_REME
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_DUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_verifica_due .

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CHECK s_cabec-numero_due IS NOT INITIAL.

  SELECT SINGLE numero_due
    FROM zsdt0170 INTO s_cabec-numero_due
   WHERE numero_due  EQ s_cabec-numero_due
     AND status      EQ '1'.

  IF sy-subrc IS NOT INITIAL.
    CLEAR s_cabec-numero_due.
    MESSAGE e836(sd) WITH text-021.
  ELSE.
*   Preenche Dados DDE/Navio
    PERFORM z_preenche_dde.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_NUMERO_DUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_numero_due INPUT.
  PERFORM z_verifica_due.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_DESVINCULAR_DUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_desvincular_due .

  DATA: vl_id_doc  TYPE zid_doc.

  IF s_cabec-id_due IS INITIAL OR s_cabec-numero_due IS INITIAL.
    MESSAGE i836(sd) WITH text-021.
    EXIT.
  ENDIF.

  SELECT SINGLE id_doc_exp
    FROM zdoc_exp INTO vl_id_doc
   WHERE vbeln EQ v_vbeln.

  CHECK ( sy-subrc EQ 0 ) AND ( vl_id_doc IS NOT INITIAL ).

  DELETE FROM zdoc_exp    WHERE id_doc_exp = vl_id_doc.
  DELETE FROM zdoc_rem_bl WHERE id_doc_exp = vl_id_doc.

  MESSAGE i836(sd) WITH text-020.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CLEAR: s_cabec.

  LEAVE TO SCREEN 0.

ENDFORM.
