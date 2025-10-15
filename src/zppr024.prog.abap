*  ----------------------------------------------------------------------*
*   ID........:                                                          *
*   Programa..: ZPPR024                                                  *
*   Tipo......: R - Report                                               *
*   Transação.: ZPM0098                                                  *
*   Descrição.: Carga da Alteração Lista Técnica                         *
*   Autor.....: Leandro Valentim Ferreira                                *
*   Data......: 27.03.2023                                               *
*  ----------------------------------------------------------------------*
*                       Controle de Alterações                           *
*  ----------------------------------------------------------------------*
*   Data       | Change     | Autor        | Alteração                   *
*  ----------------------------------------------------------------------*
*   27.03.23   |            |LVF           | Codificação Inicial         *
*  ----------------------------------------------------------------------*
REPORT zppr024.

TABLES: cdhdr.


TYPES: BEGIN OF ty_cdhdr,
         objectclas TYPE cdhdr-objectclas,
         objectid   TYPE cdhdr-objectid,
         changenr   TYPE cdhdr-changenr,
         udate      TYPE cdhdr-udate,
         utime      TYPE cdhdr-utime,
         tcode      TYPE cdhdr-tcode,
         change_ind TYPE cdhdr-change_ind,
         username   TYPE cdhdr-username,
       END OF ty_cdhdr.

DATA: t_cdhdr         TYPE TABLE OF ty_cdhdr,
      t_cdpos         TYPE TABLE OF cdpos,
      t_mpla          TYPE TABLE OF mpla,
      t_mpos          TYPE TABLE OF mpos,
      t_equi          TYPE TABLE OF equi,
      t_eqkt          TYPE TABLE OF eqkt,
      t_fleet         TYPE TABLE OF fleet,
      t_equz          TYPE TABLE OF equz,
      t_itob          TYPE TABLE OF itob,
      t_iflot         TYPE TABLE OF iflot,
      t_iloa          TYPE TABLE OF iloa,
      t_iflotx        TYPE TABLE OF iflotx,
      t_zppt_controle TYPE TABLE OF zppt_controle,
      r_tcode         TYPE RANGE OF cdhdr-tcode,
      r_warpl         TYPE RANGE OF mpla-warpl,
      r_wapos         TYPE RANGE OF mpos-wapos,
      r_equnr         TYPE RANGE OF equz-equnr,
      r_tplnr         TYPE RANGE OF iflo-tplnr.

*  ----------------------------------------------------------------------*
*   Parâmetros de Seleção
*  ----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_data FOR cdhdr-udate OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN: END OF BLOCK  bl1.

START-OF-SELECTION.
  PERFORM zf_busca_dados.
  PERFORM zf_grava_dados.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_busca_dados .

  SELECT *
         INTO TABLE t_zppt_controle
         FROM zppt_controle.

  APPEND INITIAL LINE TO r_tcode ASSIGNING FIELD-SYMBOL(<fs_tcode>).
  <fs_tcode>-sign = 'I'.
  <fs_tcode>-option = 'EQ'.
  <fs_tcode>-low = 'IE02'."Equipamento

  APPEND INITIAL LINE TO r_tcode ASSIGNING <fs_tcode>.
  <fs_tcode>-sign = 'I'.
  <fs_tcode>-option = 'EQ'.
  <fs_tcode>-low = 'IL02'."Local Instalação

  APPEND INITIAL LINE TO r_tcode ASSIGNING <fs_tcode>.
  <fs_tcode>-sign = 'I'.
  <fs_tcode>-option = 'EQ'.
  <fs_tcode>-low = 'IP02'."Plano Manutenção

  SELECT objectclas objectid changenr udate utime tcode change_ind username
         INTO TABLE t_cdhdr
         FROM cdhdr
         WHERE udate IN s_data
           AND tcode IN r_tcode.

  IF sy-subrc EQ 0.
    SORT t_cdhdr BY objectclas objectid changenr.
    SELECT *
           INTO TABLE t_cdpos
           FROM cdpos
           FOR ALL ENTRIES IN t_cdhdr
           WHERE objectclas EQ t_cdhdr-objectclas
             AND objectid   EQ t_cdhdr-objectid
             AND chngind    EQ 'U'."Alteração


    IF sy-subrc EQ 0.
      LOOP AT t_cdhdr INTO DATA(wl_cdhdr).

        CASE wl_cdhdr-tcode.
          WHEN 'IE02'."Equipamento
            LOOP AT t_cdpos INTO DATA(wl_cdpos) WHERE objectclas EQ wl_cdhdr-objectclas
                                                  AND objectid   EQ wl_cdhdr-objectid
                                                  AND changenr   EQ wl_cdhdr-changenr.

              APPEND INITIAL LINE TO r_equnr ASSIGNING FIELD-SYMBOL(<fs_equnr>).
              <fs_equnr>-sign = 'I'.
              <fs_equnr>-option = 'EQ'.
              <fs_equnr>-low = wl_cdpos-objectid.
            ENDLOOP.
            SORT r_equnr BY low.
            DELETE ADJACENT DUPLICATES FROM r_equnr COMPARING low.
          WHEN 'IP02'."Plano Manutenção
            LOOP AT t_cdpos INTO wl_cdpos WHERE objectclas EQ wl_cdhdr-objectclas
                                            AND objectid   EQ wl_cdhdr-objectid
                                            AND changenr   EQ wl_cdhdr-changenr.

              IF wl_cdhdr-objectclas EQ 'MITEM'.
                APPEND INITIAL LINE TO r_wapos ASSIGNING FIELD-SYMBOL(<fs_wapos>).
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wl_cdpos-objectid
                  IMPORTING
                    output = wl_cdpos-objectid.

                <fs_wapos>-sign = 'I'.
                <fs_wapos>-option = 'EQ'.
                <fs_wapos>-low = wl_cdpos-objectid.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <fs_wapos>-low
                  IMPORTING
                    output = <fs_wapos>-low.
              ELSE.
                APPEND INITIAL LINE TO r_warpl ASSIGNING FIELD-SYMBOL(<fs_warpl>).

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wl_cdpos-objectid
                  IMPORTING
                    output = wl_cdpos-objectid.

                <fs_warpl>-sign = 'I'.
                <fs_warpl>-option = 'EQ'.
                <fs_warpl>-low = wl_cdpos-objectid.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <fs_warpl>-low
                  IMPORTING
                    output = <fs_warpl>-low.
              ENDIF.

            ENDLOOP.
            SORT r_warpl BY low.
            DELETE ADJACENT DUPLICATES FROM r_warpl COMPARING low.
          WHEN 'IL02'."Local de Instalação
            LOOP AT t_cdpos INTO wl_cdpos WHERE objectclas EQ wl_cdhdr-objectclas
                                            AND objectid   EQ wl_cdhdr-objectid
                                            AND changenr   EQ wl_cdhdr-changenr.

              APPEND INITIAL LINE TO r_tplnr ASSIGNING FIELD-SYMBOL(<fs_tplnr>).
              <fs_tplnr>-sign = 'I'.
              <fs_tplnr>-option = 'EQ'.
              <fs_tplnr>-low = wl_cdpos-objectid.
            ENDLOOP.
            SORT r_tplnr BY low.
            DELETE ADJACENT DUPLICATES FROM r_tplnr COMPARING low.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF r_warpl[] IS NOT INITIAL.
    SELECT *
           INTO TABLE t_mpla
           FROM mpla
           WHERE warpl IN r_warpl.

    IF sy-subrc EQ 0.
      SELECT *
             INTO TABLE t_mpos
             FROM mpos
             FOR ALL ENTRIES IN t_mpla
             WHERE warpl EQ t_mpla-warpl.

      IF sy-subrc EQ 0.
        SORT t_mpos BY warpl aedat DESCENDING.
        DELETE ADJACENT DUPLICATES FROM t_mpos COMPARING warpl aedat.
      ENDIF.
    ENDIF.
  ENDIF.

  IF r_wapos[] IS NOT INITIAL.
    SELECT *
           APPENDING TABLE t_mpos
           FROM mpos
           WHERE wapos IN r_wapos.

    IF sy-subrc EQ 0.
      SELECT *
             APPENDING TABLE t_mpla
             FROM mpla
             FOR ALL ENTRIES IN t_mpos
             WHERE warpl EQ t_mpos-warpl.
    ENDIF.
  ENDIF.

  IF r_equnr[] IS NOT INITIAL.
    SELECT *
           INTO TABLE t_equi
           FROM equi
           WHERE equnr IN r_equnr.

    IF sy-subrc EQ 0.
      SELECT *
             INTO TABLE t_eqkt
             FROM eqkt
             FOR ALL ENTRIES IN t_equi
             WHERE equnr EQ t_equi-equnr
               AND spras EQ sy-langu.

      SELECT *
             INTO TABLE t_fleet
             FROM fleet
             FOR ALL ENTRIES IN t_equi
             WHERE objnr EQ t_equi-objnr.

***      SELECT *
***             INTO TABLE t_equz
***             FROM equz
***             FOR ALL ENTRIES IN t_equi
***             WHERE equnr EQ t_equi-equnr
***               AND aedat IN s_data.

      SELECT *
             INTO TABLE t_itob
             FROM itob
             FOR ALL ENTRIES IN t_equi
             WHERE equnr EQ t_equi-equnr
               AND aedat IN s_data.
    ENDIF.
  ENDIF.

  IF r_tplnr[] IS NOT INITIAL.
    SELECT *
           INTO TABLE t_iflot
           FROM iflot
           WHERE tplnr IN r_tplnr.

    IF sy-subrc EQ 0.
      SELECT *
             INTO TABLE t_iflotx
             FROM iflotx
             FOR ALL ENTRIES IN t_iflot
             WHERE tplnr EQ t_iflot-tplnr
               AND spras EQ sy-langu.

      SELECT *
             INTO TABLE t_iloa
             FROM iloa
             FOR ALL ENTRIES IN t_iflot
             WHERE iloan EQ t_iflot-iloan
               AND tplnr EQ t_iflot-tplnr.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_grava_dados .

  IF t_mpla[] IS NOT INITIAL OR
     t_equi[] IS NOT INITIAL OR
     t_iflot[] IS NOT INITIAL.

    IF t_mpla[] IS NOT INITIAL.
      PERFORM zf_grava_plano.
    ENDIF.
    IF t_equi[] IS NOT INITIAL.
      PERFORM zf_grava_equi.
    ENDIF.
    IF t_iflot[] IS NOT INITIAL.
      PERFORM zf_grava_local.
    ENDIF.

  ELSE.
*    MESSAGE ID 'ZPMMSG' TYPE 'I' NUMBER '000' WITH 'Dados não encontrados.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_grava_plano .

  DATA: t_zppt_plano_manu TYPE TABLE OF zppt_plano_manu,
        vl_cod_alt        TYPE zppt_plano_manu-cod_alt,
        vl_texto_campo    TYPE fieldname,
        vl_grava          TYPE flag.

* Obter os index
  SELECT * FROM zppt_plano_manu
         INTO TABLE @DATA(tl_zppt_plano_manu)
         FOR ALL ENTRIES IN @t_mpos
         WHERE werks EQ @t_mpos-iwerk
           AND warpl EQ @t_mpos-warpl.

  SORT tl_zppt_plano_manu BY werks warpl cod_alt DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zppt_plano_manu COMPARING werks warpl.

  SORT t_cdhdr BY objectclas objectid changenr.

  LOOP AT t_mpla INTO DATA(wl_mpla).
    vl_grava = abap_false.
    LOOP AT t_mpos INTO DATA(wl_mpos) WHERE warpl EQ wl_mpla-warpl.
*     Considerar somente os documentos que estão na tabela de controle
      READ TABLE t_zppt_controle INTO DATA(wl_zppt_controle) WITH KEY werks = wl_mpos-iwerk
                                                                      mptyp = wl_mpla-mptyp.
      IF sy-subrc EQ 0.
        vl_grava = abap_true.
        ADD 1 TO vl_cod_alt.
        AT NEW iwerk.
          READ TABLE tl_zppt_plano_manu INTO DATA(wl_zppt_plano_manu) WITH KEY werks = wl_mpos-iwerk
                                                                               warpl = wl_mpos-warpl
                                                                               BINARY SEARCH.
          IF sy-subrc EQ 0.
            vl_cod_alt = wl_zppt_plano_manu-cod_alt + 1.
          ENDIF.
        ENDAT.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF vl_grava EQ abap_true.

      READ TABLE t_mpos INTO wl_mpos WITH KEY warpl = wl_mpla-warpl.
      IF sy-subrc EQ 0.
        PERFORM zf_compor_zppt_plano_manu TABLES t_zppt_plano_manu USING wl_mpos-wapos vl_cod_alt wl_mpos.
      ENDIF.

      PERFORM zf_compor_zppt_plano_manu TABLES t_zppt_plano_manu USING wl_mpla-warpl vl_cod_alt wl_mpos.


***      LOOP AT t_cdpos INTO DATA(wl_cdpos) WHERE objectid EQ wl_mpla-warpl.
***        CLEAR vl_texto_campo.
***        CASE wl_cdpos-fname.
***          WHEN 'WPTXT'.
***            vl_texto_campo = 'Texto cabeçalho do plano'.
***          WHEN 'EQUNR'.
***            vl_texto_campo = 'Equipamento'.
***          WHEN 'CALL_CONFIRM'.
***            vl_texto_campo = 'Confirmação Obrigatória'.
***          WHEN 'HORIZ'.
***            vl_texto_campo = 'Horizonte de Abertura'.
***          WHEN 'AUART'.
***            vl_texto_campo = 'Tipo de Ordem'.
***          WHEN 'BAUTL'.
***            vl_texto_campo = 'Conjunto'.
***          WHEN 'PAK_TEXT'.
***            vl_texto_campo = 'Ciclo de manutenção'.
***          WHEN 'ZEIEH'.
***            vl_texto_campo = 'Unidade de medida'.
***        ENDCASE.
***
***        IF vl_texto_campo IS NOT INITIAL.
***
***          READ TABLE t_cdhdr INTO DATA(wl_cdhdr) WITH KEY objectclas = wl_cdpos-objectclas
***                                                          objectid   = wl_cdpos-objectid
***                                                          changenr   = wl_cdpos-changenr
***                                                          BINARY SEARCH.
***          IF sy-subrc EQ 0.
***            APPEND INITIAL LINE TO t_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).
***            <fs_zppt_plano_manu>-werks         = wl_mpos-iwerk.
***            <fs_zppt_plano_manu>-warpl         = wl_mpos-warpl.
***            <fs_zppt_plano_manu>-cod_alt       = vl_cod_alt.
***            <fs_zppt_plano_manu>-field         = vl_texto_campo.
***            <fs_zppt_plano_manu>-value_new     = wl_cdpos-value_new.
***            <fs_zppt_plano_manu>-value_old     = wl_cdpos-value_old.
***            <fs_zppt_plano_manu>-aedat         = wl_cdhdr-udate.
***            <fs_zppt_plano_manu>-hr_alteracao  = wl_cdhdr-utime.
***            <fs_zppt_plano_manu>-aenam         = wl_cdhdr-username.
***
***            <fs_zppt_plano_manu>-status        = 'P'.
***            ADD 1 TO vl_cod_alt.
***          ENDIF.
***
***          CLEAR wl_cdhdr.
***        ENDIF.
***      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF t_zppt_plano_manu[] IS NOT INITIAL.


    "FF 12.02.24 - USER STORY 78112 - inicio
    SELECT *
    FROM zppt_plano_manu
    INTO TABLE @DATA(lt_plano_manu).

    IF sy-subrc = 0.
      LOOP AT lt_plano_manu INTO DATA(wa).

        READ TABLE t_zppt_plano_manu WITH KEY  werks = wa-werks
                                               warpl = wa-warpl
                                               field = wa-field
                                               value_new = wa-value_new
                                               value_old = wa-value_old TRANSPORTING NO FIELDS.

        IF sy-subrc = 0.
          DELETE t_zppt_plano_manu INDEX sy-tabix.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "FF 12.02.24 - USER STORY 78112 - fim



    PERFORM zf_email_manut TABLES t_zppt_plano_manu USING wl_zppt_controle.
    LOOP AT t_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).
      <fs_zppt_plano_manu>-status_email = 'S'.
    ENDLOOP.
    MODIFY zppt_plano_manu FROM TABLE t_zppt_plano_manu.
    COMMIT WORK AND WAIT.

    MESSAGE ID 'ZPMMSG' TYPE 'S' NUMBER '000' WITH 'Dados processado com sucesso.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_EQUI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_grava_equi .

  DATA: t_zppt_inter_equip TYPE TABLE OF zppt_inter_equip,
        vl_cod_alt         TYPE zppt_plano_manu-cod_alt,
        vl_texto_campo     TYPE fieldname,
        vl_grava           TYPE flag.

* Obter os index
  SELECT * FROM zppt_inter_equip
         INTO TABLE @DATA(tl_zppt_inter_equip)
         FOR ALL ENTRIES IN @t_equi
         WHERE "werks EQ @t_equi-werk
               equnr EQ @t_equi-equnr.

  SORT tl_zppt_inter_equip BY werks equnr cod_alt DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zppt_inter_equip COMPARING werks equnr.

  SORT t_cdhdr BY objectclas objectid changenr.

  LOOP AT t_equi INTO DATA(wl_equi).
    vl_grava = abap_false.
    READ TABLE t_itob INTO DATA(wl_itob) WITH KEY equnr = wl_equi-equnr.
    IF sy-subrc EQ 0.
*     Considerar somente os documentos que estão na tabela de controle
      READ TABLE t_zppt_controle INTO DATA(wl_zppt_controle) WITH KEY werks = wl_itob-iwerk
                                                                      eqtyp = wl_equi-eqtyp.
      IF sy-subrc EQ 0.
        vl_grava = abap_true."Grava pois atendeu a tabela de controle
        READ TABLE t_fleet INTO DATA(wl_fleet) WITH KEY objnr = wl_equi-objnr.
        READ TABLE t_eqkt  INTO DATA(wl_eqkt) WITH KEY equnr = wl_equi-equnr.

        ADD 1 TO vl_cod_alt.
        READ TABLE tl_zppt_inter_equip INTO DATA(wl_zppt_inter_equip) WITH KEY werks = wl_itob-iwerk
                                                                               equnr = wl_equi-equnr
                                                                               BINARY SEARCH.
        IF sy-subrc EQ 0.
          vl_cod_alt = wl_zppt_inter_equip-cod_alt + 1.
        ENDIF.

        LOOP AT t_cdpos INTO DATA(wl_cdpos) WHERE objectid EQ wl_equi-equnr.

          CLEAR vl_texto_campo.
          CASE wl_cdpos-fname.
            WHEN 'EQKTX'.
              vl_texto_campo = 'Descrição objeto Técnico'.
            WHEN 'HERST'.
              vl_texto_campo = 'Fabricante'.
            WHEN 'TYPBZ'.
              vl_texto_campo = 'Modelo'.
            WHEN 'RBNR'.
              vl_texto_campo = 'Perfil Catalogo'.
            WHEN 'FLEET_CAT'.
              vl_texto_campo = 'Tipo de objeto'.
            WHEN 'CHASSIS_NUM'.
              vl_texto_campo = 'Chassis'.
            WHEN 'LICENSE_NUM'.
              vl_texto_campo = 'Placa'.
            WHEN 'ABCKZ'.
              vl_texto_campo = 'ABC'.
          ENDCASE.

          IF vl_texto_campo IS NOT INITIAL.

            READ TABLE t_cdhdr INTO DATA(wl_cdhdr) WITH KEY objectclas = wl_cdpos-objectclas
                                                            objectid   = wl_cdpos-objectid
                                                            changenr   = wl_cdpos-changenr
                                                            BINARY SEARCH.
            IF sy-subrc EQ 0.
              APPEND INITIAL LINE TO t_zppt_inter_equip ASSIGNING FIELD-SYMBOL(<fs_zppt_inter_equip>).
              <fs_zppt_inter_equip>-werks        = wl_itob-iwerk.
              <fs_zppt_inter_equip>-equnr        = wl_equi-equnr.
              <fs_zppt_inter_equip>-cod_alt      = vl_cod_alt.
              <fs_zppt_inter_equip>-field        = vl_texto_campo.
              <fs_zppt_inter_equip>-value_new    = wl_cdpos-value_new.
              <fs_zppt_inter_equip>-value_old    = wl_cdpos-value_old.
              <fs_zppt_inter_equip>-aedat        = wl_cdhdr-udate.
              <fs_zppt_inter_equip>-hr_alteracao = wl_cdhdr-utime.
              <fs_zppt_inter_equip>-aenam        = wl_cdhdr-username.
              <fs_zppt_inter_equip>-status       = 'P'.
              ADD 1 TO vl_cod_alt.
            ENDIF.

            CLEAR wl_cdhdr.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF t_zppt_inter_equip[] IS NOT INITIAL.

    "FF 12.02.24 - USER STORY 78112 - inicio
    SELECT *
    FROM zppt_inter_equip
    INTO TABLE @DATA(lt_inter_equip).

    IF sy-subrc = 0.
      LOOP AT lt_inter_equip INTO DATA(wa).

        READ TABLE t_zppt_inter_equip WITH KEY werks = wa-werks
                                               equnr = wa-equnr
                                               field = wa-field
                                               value_new = wa-value_new
                                               value_old = wa-value_old TRANSPORTING NO FIELDS.

        IF sy-subrc = 0.
          DELETE t_zppt_inter_equip INDEX sy-tabix.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "FF 12.02.24 - USER STORY 78112 - fim


    PERFORM zf_email_equip TABLES t_zppt_inter_equip USING wl_zppt_controle.
    LOOP AT t_zppt_inter_equip ASSIGNING <fs_zppt_inter_equip>.
      <fs_zppt_inter_equip>-status_email = 'S'.
    ENDLOOP.
    MODIFY zppt_inter_equip FROM TABLE t_zppt_inter_equip.
    COMMIT WORK AND WAIT.

    MESSAGE ID 'ZPMMSG' TYPE 'S' NUMBER '000' WITH 'Dados processado com sucesso.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EMAIL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_email_equip  TABLES p_lista STRUCTURE zppt_inter_equip
                     USING p_zppt_controle TYPE zppt_controle.

  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt_inter_equip.
  TYPES:   maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
           email           TYPE ad_smtpadr,
         END OF y_itens.

  DATA: tl_itens TYPE TABLE OF y_itens.

  DATA: wl_itens LIKE LINE OF tl_itens.

  CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

  DATA: lv_bukrs            TYPE bukrs,
        lv_rec_cnt          TYPE c LENGTH 10,
        lv_lines            TYPE i,
        lv_sentall          TYPE sonv-flag,
        lv_nobjid           TYPE sofolenti1-object_id,
        lv_aedat(10)        TYPE c,
        lv_hr_alteracao(10) TYPE c,
        lv_qtd_antiga(20)   TYPE c,
        lv_qtd_atual(20)    TYPE c.

  DATA: tl_objtxt   TYPE TABLE OF solisti1,
        tl_objpack  TYPE TABLE OF sopcklsti1,
        tl_receiver TYPE TABLE OF somlreci1.

  DATA: wl_docdata  TYPE sodocchgi1,
        wl_objtxt   LIKE LINE OF tl_objtxt,
        wl_objpack  LIKE LINE OF tl_objpack,
        wl_receiver LIKE LINE OF tl_receiver.

  LOOP AT p_lista INTO DATA(wl_lista).
    MOVE-CORRESPONDING wl_lista TO wl_itens.

*   Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.
      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.
    ENDIF.

    IF wl_itens-werks IS NOT INITIAL AND p_zppt_controle-eqtyp IS NOT INITIAL.
      SELECT * FROM zppt_controle
        INTO TABLE @DATA(tl_zppt_controle)
         WHERE werks EQ @wl_itens-werks
           AND eqtyp EQ @p_zppt_controle-eqtyp.
    ENDIF.
    APPEND wl_itens TO tl_itens.
    CLEAR wl_itens.
  ENDLOOP.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
        INTO wl_objtxt-line SEPARATED BY space.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Equipamento </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Descrição Campo Alterado </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Conteúdo antigo do campo </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Novo conteúdo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH> Responsável modificação </TH>' TO wl_objtxt-line.     "FF 12.02.24 - USER STORY 78112
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.



  LOOP AT tl_itens INTO wl_itens.

*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    data(lv_equnr) = |{ wl_itens-equnr ALPHA = OUT }| .       "FF 12.02.24 - USER STORY 78112

    CONCATENATE '<TD>' lv_equnr '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-field '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_old '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_new '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col10
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col11
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col12
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  LOOP AT tl_zppt_controle INTO DATA(wl_zppt_controle) WHERE werks EQ wl_itens-werks.
    IF wl_zppt_controle-email IS NOT INITIAL.
      wl_receiver-receiver = wl_zppt_controle-email.
      wl_receiver-rec_type = 'U'.
      wl_receiver-com_type = c_comtyp_int.
      APPEND wl_receiver TO tl_receiver.
      CLEAR wl_receiver.
    ENDIF.
  ENDLOOP.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  CLEAR: tl_receiver[], tl_objtxt[], tl_objpack[], lv_nobjid, lv_sentall, wl_docdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EMAIL_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_email_manut  TABLES p_lista STRUCTURE zppt_plano_manu
                     USING p_zppt_controle TYPE zppt_controle.

  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt_plano_manu.
  TYPES:   maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
           email           TYPE ad_smtpadr,
         END OF y_itens.

  DATA: tl_itens TYPE TABLE OF y_itens.

  DATA: wl_itens LIKE LINE OF tl_itens.

  CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

  DATA: lv_bukrs            TYPE bukrs,
        lv_rec_cnt          TYPE c LENGTH 10,
        lv_lines            TYPE i,
        lv_sentall          TYPE sonv-flag,
        lv_nobjid           TYPE sofolenti1-object_id,
        lv_aedat(10)        TYPE c,
        lv_hr_alteracao(10) TYPE c,
        lv_qtd_antiga(20)   TYPE c,
        lv_qtd_atual(20)    TYPE c.

  DATA: tl_objtxt   TYPE TABLE OF solisti1,
        tl_objpack  TYPE TABLE OF sopcklsti1,
        tl_receiver TYPE TABLE OF somlreci1.

  DATA: wl_docdata  TYPE sodocchgi1,
        wl_objtxt   LIKE LINE OF tl_objtxt,
        wl_objpack  LIKE LINE OF tl_objpack,
        wl_receiver LIKE LINE OF tl_receiver.


  LOOP AT p_lista INTO DATA(wl_lista).
    MOVE-CORRESPONDING wl_lista TO wl_itens.

*     Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.
      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.
    ENDIF.

    IF wl_itens-werks IS NOT INITIAL AND p_zppt_controle-mptyp IS NOT INITIAL.
      SELECT * FROM zppt_controle
        INTO TABLE @DATA(tl_zppt_controle)
         WHERE werks EQ @p_zppt_controle-werks
           AND mptyp EQ @p_zppt_controle-mptyp.
    ENDIF.
    APPEND wl_itens TO tl_itens.
    CLEAR wl_itens.

  ENDLOOP.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
        INTO wl_objtxt-line SEPARATED BY space.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Plano </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Descrição Campo Alterado </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Conteúdo antigo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Novo conteúdo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH> Responsável modificação </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  LOOP AT tl_itens INTO wl_itens.

*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    data(lv_warpl) = |{ wl_itens-warpl ALPHA = OUT }| .       "FF 12.02.24 - USER STORY 78112

    CONCATENATE '<TD>' lv_warpl '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-field '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_old '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_new '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col10
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col11
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col12
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  LOOP AT tl_zppt_controle INTO DATA(wl_zppt_controle) WHERE werks EQ wl_itens-werks.
    IF wl_zppt_controle-email IS NOT INITIAL.
      wl_receiver-receiver = wl_zppt_controle-email.
      wl_receiver-rec_type = 'U'.
      wl_receiver-com_type = c_comtyp_int.
      APPEND wl_receiver TO tl_receiver.
      CLEAR wl_receiver.
    ENDIF.
  ENDLOOP.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  CLEAR: tl_receiver[], tl_objtxt[], tl_objpack[], lv_nobjid, lv_sentall, wl_docdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_grava_local.

***  DATA: t_zppt_loc_instal TYPE TABLE OF zppt_loc_instal,
***        vl_cod_alt        TYPE zppt_loc_instal-cod_alt,
***        vl_texto_campo    TYPE fieldname,
***        vl_grava          TYPE flag.
***
**** Obter os index
***  SELECT * FROM zppt_loc_instal
***         INTO TABLE @DATA(tl_zppt_loc_instal)
***         FOR ALL ENTRIES IN @t_iflot
***         WHERE werks EQ @t_iflot-iwerk
***           AND tplnr EQ @t_iflot-tplnr.
***
***  SORT tl_zppt_loc_instal BY werks tplnr cod_alt DESCENDING.
***  DELETE ADJACENT DUPLICATES FROM tl_zppt_loc_instal COMPARING werks tplnr.
***
***  SORT t_cdhdr BY objectclas objectid changenr.
***
***  LOOP AT t_iflot INTO DATA(wl_iflot).
***    vl_grava = abap_false.
****   Considerar somente os documentos que estão na tabela de controle
***    READ TABLE t_zppt_controle INTO DATA(wl_zppt_controle) WITH KEY werks = wl_iflot-iwerk
***                                                                    fltyp = wl_iflot-fltyp.
***    IF sy-subrc EQ 0.
***      vl_grava = abap_true.
***      ADD 1 TO vl_cod_alt.
***      READ TABLE tl_zppt_loc_instal INTO DATA(wl_zppt_loc_instal) WITH KEY werks = wl_iflot-iwerk
***                                                                           tplnr = wl_iflot-tplnr
***                                                                           BINARY SEARCH.
***      IF sy-subrc EQ 0.
***        vl_cod_alt = wl_zppt_loc_instal-cod_alt + 1.
***      ENDIF.
***    ENDIF.
***
***    IF vl_grava EQ abap_true.
***      LOOP AT t_cdpos INTO DATA(wl_cdpos) WHERE objectid EQ wl_iflot-tplnr.
***        CLEAR vl_texto_campo.
***        CASE wl_cdpos-fname.
***          WHEN 'PLTXT'.
***            vl_texto_campo = 'Denominação'.
***          WHEN 'BEGRU'.
***            vl_texto_campo = 'Grupo Autorização'.
***          WHEN 'IWERK'.
***            vl_texto_campo = ' Centro'.
***          WHEN 'KOSTL'.
***            vl_texto_campo = 'Centro de Custo'.
***          WHEN 'GSBER'.
***            vl_texto_campo = 'Divisão'.
***          WHEN 'ABCKZ'.
***            vl_texto_campo = 'ABC'.
***          WHEN 'BEBER'.
***            vl_texto_campo = 'Área Operacional'.
***        ENDCASE.
***
***        IF vl_texto_campo IS NOT INITIAL.
***
***          READ TABLE t_cdhdr INTO DATA(wl_cdhdr) WITH KEY objectclas = wl_cdpos-objectclas
***                                                          objectid   = wl_cdpos-objectid
***                                                          changenr   = wl_cdpos-changenr
***                                                          BINARY SEARCH.
***          IF sy-subrc EQ 0.
***            APPEND INITIAL LINE TO t_zppt_loc_instal ASSIGNING FIELD-SYMBOL(<fs_zppt_loc_instal>).
***            <fs_zppt_loc_instal>-werks       = wl_iflot-iwerk.
***            <fs_zppt_loc_instal>-tplnr       = wl_iflot-tplnr.
***            <fs_zppt_loc_instal>-cod_alt     = vl_cod_alt.
***            <fs_zppt_loc_instal>-field       = vl_texto_campo.
***            <fs_zppt_loc_instal>-value_new   = wl_cdpos-value_new.
***            <fs_zppt_loc_instal>-value_old   = wl_cdpos-value_old.
***            <fs_zppt_loc_instal>-aedat       = wl_cdhdr-udate.
***            <fs_zppt_loc_instal>-aenam       = wl_cdhdr-utime.
***            <fs_zppt_loc_instal>-status = 'P'.
***            ADD 1 TO vl_cod_alt.
***          ENDIF.
***
***          CLEAR wl_cdhdr.
***        ENDIF.
***      ENDLOOP.
***    ENDIF.

***  ENDLOOP.
***
***  IF t_zppt_loc_instal[] IS NOT INITIAL.
******    PERFORM zf_email_instal TABLES t_zppt_loc_instal.
***    LOOP AT t_zppt_loc_instal ASSIGNING <fs_zppt_loc_instal>.
***      <fs_zppt_loc_instal>-status_email = 'S'.
***    ENDLOOP.
***    MODIFY zppt_loc_instal FROM TABLE t_zppt_loc_instal.
***    COMMIT WORK AND WAIT.
***
***    MESSAGE ID 'ZPMMSG' TYPE 'S' NUMBER '000' WITH 'Dados processado com sucesso.'.
***  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_COMPOR_ZPPT_PLANO_MANU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_compor_zppt_plano_manu TABLES p_zppt_plano_manu STRUCTURE zppt_plano_manu
                               USING p_obj p_cod_alt p_mpos TYPE mpos.


  DATA: vl_texto_campo TYPE fieldname,
        vl_zeieh_new   TYPE dzeieh,
        vl_zeieh_old   TYPE dzeieh,
        vl_mseh3_new   TYPE t006a-mseh3,
        vl_mseh3_old   TYPE t006a-mseh3.

  LOOP AT t_cdpos INTO DATA(wl_cdpos) WHERE objectid EQ p_obj.
    CLEAR vl_texto_campo.
    CASE wl_cdpos-fname.
      WHEN 'WPTXT'.
        vl_texto_campo = 'Texto cabeçalho do plano'.
      WHEN 'EQUNR'.
        vl_texto_campo = 'Equipamento'.
      WHEN 'CALL_CONFIRM'.
        vl_texto_campo = 'Confirmação Obrigatória'.
      WHEN 'HORIZ'.
        vl_texto_campo = 'Horizonte de Abertura'.
      WHEN 'AUART'.
        vl_texto_campo = 'Tipo de Ordem'.
      WHEN 'BAUTL'.
        vl_texto_campo = 'Conjunto'.
      WHEN 'PAK_TEXT'.
        vl_texto_campo = 'Ciclo de manutenção'.
      WHEN 'ZEIEH'.
        vl_texto_campo = 'Unidade de medida'.
        vl_zeieh_new = wl_cdpos-value_new.
        vl_zeieh_old = wl_cdpos-value_old.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = vl_zeieh_new
          IMPORTING
            output         = vl_mseh3_new
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        wl_cdpos-value_new = vl_mseh3_new.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = vl_zeieh_old
          IMPORTING
            output         = vl_mseh3_old
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        wl_cdpos-value_old = vl_mseh3_old.
    ENDCASE.

    IF vl_texto_campo IS NOT INITIAL.

      READ TABLE t_cdhdr INTO DATA(wl_cdhdr) WITH KEY objectclas = wl_cdpos-objectclas
                                                      objectid   = wl_cdpos-objectid
                                                      changenr   = wl_cdpos-changenr
                                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO p_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).
        <fs_zppt_plano_manu>-werks         = p_mpos-iwerk.
        <fs_zppt_plano_manu>-warpl         = p_mpos-warpl.
        <fs_zppt_plano_manu>-cod_alt       = p_cod_alt.
        <fs_zppt_plano_manu>-field         = vl_texto_campo.
        <fs_zppt_plano_manu>-value_new     = wl_cdpos-value_new.
        <fs_zppt_plano_manu>-value_old     = wl_cdpos-value_old.
        <fs_zppt_plano_manu>-aedat         = wl_cdhdr-udate.
        <fs_zppt_plano_manu>-hr_alteracao  = wl_cdhdr-utime.
        <fs_zppt_plano_manu>-aenam         = wl_cdhdr-username.

        <fs_zppt_plano_manu>-status        = 'P'.
        ADD 1 TO p_cod_alt.
      ENDIF.

      CLEAR wl_cdhdr.
    ENDIF.
  ENDLOOP.

ENDFORM.
