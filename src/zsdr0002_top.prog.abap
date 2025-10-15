*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZSDR0002_TOP                                                                         *
*& Chamado        : USER STORY 168894                                                                    *
*& Data           : 18/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 18/03/2025|DEVK9A1XAW |NSEGATIN       | Cadastro de Aprovador 1x1. Desenvolvimento inicial.           *
*--------------------------------------------------------------------------------------------------------*
TYPE-POOLS icon.

TYPES: BEGIN OF ty_saida,
         aprovador  TYPE xubname,
         bukrs      TYPE bukrs,
         name_text  TYPE ad_namtext,
         grp_email  TYPE ad_smtpadr,
         ativo      TYPE zde_registro_ativo,
         us_criacao TYPE zuname,
         dt_criacao TYPE sydatum,
         hr_criacao TYPE syuzeit,
         celltab    TYPE lvc_t_styl,
         row_id     TYPE int4,
         new_line   TYPE c,
       END OF ty_saida.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida        TYPE TABLE OF ty_saida,
      it_saida_update TYPE TABLE OF ty_saida,
      wa_saida        TYPE          ty_saida,
      tg_vinc_aprov   TYPE TABLE OF zsdtvinc_aprov,
      eg_vinc_aprov   TYPE          zsdtvinc_aprov.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: gv_erro  TYPE c,
      gv_modif TYPE c.

**---------------------------------------------------------------------*
**  Inicio Implementação Classes
**---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_data_changed
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING et_good_cells.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA(tl_toolbar) = e_object->mt_toolbar.

    READ TABLE tl_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>) WITH KEY function = '&LOCAL&INSERT_ROW'.

    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'INS'.
      e_object->mt_toolbar = tl_toolbar.

    ENDIF.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'DEL'.
* Excluir linha do ALV Grid
        PERFORM deletar_reg.

      WHEN 'INS'.
        APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
        <fs_saida>-ativo    = abap_on.
        <fs_saida>-row_id   = sy-tabix.
        <fs_saida>-new_line = abap_on.
        CALL METHOD obj_alv->refresh_table_display( ).

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lt_cell  TYPE TABLE OF lvc_s_ceno,
          lwa_cell LIKE LINE OF lt_cell.

    DATA: vl_row_id TYPE int4.

    IF et_good_cells IS NOT INITIAL.

      gv_modif = abap_true.
* Determina só o campos APROVADOR para a busca dos nome e sobrenome para compor o nome completo.
      DATA(lt_good) = et_good_cells.
      DELETE lt_good WHERE fieldname EQ 'NAME_TEXT'
                        OR fieldname EQ 'US_CRIACAO'
                        OR fieldname EQ 'DT_CRIACAO'
                        OR fieldname EQ 'HR_CRIACAO'.
* Excluí os novos registros para controle.
      DATA(lt_saida) = it_saida.
      DELETE lt_saida WHERE name_text IS INITIAL.
      SORT lt_saida BY aprovador bukrs.
* Excluí os registros já existentes para controle.
      DATA(lt_saida2) = it_saida.
      DELETE lt_saida2 WHERE name_text IS NOT INITIAL.
      SORT lt_saida2 BY aprovador bukrs.
* Verifica se há dados para serem processados.
      IF lt_good IS NOT INITIAL.
* Seleciona o Nome e o sobrenome conforme o ID do Usuário.
        SELECT a~bname, b~name_first, b~name_last
          FROM usr21 AS a
          INNER JOIN adrp AS b
           ON b~persnumber = a~persnumber
          INTO TABLE @DATA(lt_usr21)
          FOR ALL ENTRIES IN @lt_good
        WHERE a~bname EQ @lt_good-value(12).

        IF sy-subrc IS INITIAL.
          SORT lt_usr21 BY bname.

        ENDIF.

        LOOP AT lt_good ASSIGNING FIELD-SYMBOL(<fs_changed>).
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_changed>-row_id.
          CHECK <fs_saida> IS ASSIGNED.
* Objeto de autorização Autorização Aprovadores 1x1
          AUTHORITY-CHECK OBJECT 'ZSD_1X1_AP' ID 'BUKRS' FIELD <fs_saida>-bukrs.

          IF NOT sy-subrc         IS INITIAL OR
                 <fs_saida>-bukrs IS INITIAL.
* Carrega a posição da célula de linha e coluna que foi criticada com falta de autorização para destacar
* na Grid do ALV.
            lwa_cell-col_id = 3.
            lwa_cell-row_id = sy-tabix.
            APPEND lwa_cell TO lt_cell.

            IF <fs_saida>-bukrs IS INITIAL.
              DATA(vl_err_auto) = 1. "Empresa obrigatório

            ELSE.
              vl_err_auto = 2.       "Sem autorização

            ENDIF.

          ENDIF.

          IF vl_row_id NE <fs_changed>-row_id.
            vl_row_id         = <fs_changed>-row_id.
            DATA(vl_equal)    = abap_on.

          ELSE.
            CONTINUE.

          ENDIF.

          CASE <fs_changed>-fieldname.
            WHEN 'APROVADOR'. "ID do usuário
              READ TABLE lt_usr21 ASSIGNING FIELD-SYMBOL(<fs_usr21>) WITH KEY bname = <fs_changed>-value BINARY SEARCH.

              IF sy-subrc IS INITIAL.
                READ TABLE lt_saida TRANSPORTING NO FIELDS WITH KEY aprovador = <fs_saida>-aprovador
                                                                    bukrs     = <fs_saida>-bukrs
                                                           BINARY SEARCH.

                IF sy-subrc IS INITIAL.
                  READ TABLE lt_saida2 TRANSPORTING NO FIELDS WITH KEY aprovador = <fs_saida>-aprovador
                                                                       bukrs     = <fs_saida>-bukrs
                                                              BINARY SEARCH.

                  IF sy-subrc IS INITIAL.
* Carrega a posição da célula de linha e coluna que foi criticada com duplicidade de Aprovador
* para destacar na Grid do ALV.
                    lwa_cell-col_id = 1.
                    lwa_cell-row_id = sy-tabix.
                    APPEND lwa_cell TO lt_cell.
                    gv_erro = abap_true.

                  ENDIF.

                ELSE.
                  CONCATENATE <fs_usr21>-name_first <fs_usr21>-name_last INTO <fs_saida>-name_text SEPARATED BY space.

                ENDIF.

              ENDIF.

            WHEN OTHERS.
*           Do nothing
          ENDCASE.

          IF NOT vl_equal IS INITIAL AND
                 gv_erro  IS INITIAL.
            READ TABLE it_saida_update ASSIGNING FIELD-SYMBOL(<fs_saida_update>) WITH KEY aprovador = <fs_saida>-aprovador
                                                                                          bukrs     = <fs_saida>-bukrs.

            IF sy-subrc IS INITIAL.
              <fs_saida_update> = <fs_saida>.

            ELSE.
              APPEND <fs_saida> TO it_saida_update.

            ENDIF.

            CLEAR vl_equal.

          ENDIF.

        ENDLOOP.

        CALL METHOD obj_alv->refresh_table_display( ).
* Verifica se há celulas da Grid ALV para marcar que foram criticadas.
        CHECK lt_cell IS NOT INITIAL.
* Marca as celulas da Grid ALV que foram criticadas.
        CALL METHOD obj_alv->set_selected_cells_id
          EXPORTING
            it_cells = lt_cell.
* Verifica se houve erro no Objeto de Autorização da Empresa.
        IF vl_err_auto IS INITIAL.
          MESSAGE 'Já existe um cadastro com este Aprovador' TYPE 'S' DISPLAY LIKE 'E'.

        ELSE.
          CASE vl_err_auto.
            WHEN 1. "Empresa Obrigatória
              MESSAGE 'Empresa é obrigatória' TYPE 'S' DISPLAY LIKE 'E'.

            WHEN 2. "Sem Autorização
              MESSAGE 'Sem autorização para essa(s) Empresa(s)' TYPE 'S' DISPLAY LIKE 'E'.

            WHEN OTHERS.
*           Do nothing
          ENDCASE.

          gv_erro = abap_true.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: obj_toolbar      TYPE REF TO lcl_alv_toolbar.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.
