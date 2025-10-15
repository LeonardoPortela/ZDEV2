*&---------------------------------------------------------------------*
*&  Include           MZPMR0066FORM
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
form EXIT_PROGRAM.
  leave program.
endform.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
form BUILD_FIELDCAT changing PT_FIELDCAT type LVC_T_FCAT.

  data LS_FCAT type LVC_S_FCAT.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      I_STRUCTURE_NAME = 'ZPMS0040'
    changing
      CT_FIELDCAT      = PT_FIELDCAT.


  loop at PT_FIELDCAT into LS_FCAT.


    LS_FCAT-CHECKTABLE = '!'.        "do not check foreign keys

    if LS_FCAT-FIELDNAME = 'OBSERV'.
      LS_FCAT-NO_OUT = 'X'.
    endif.

    if LS_FCAT-FIELDNAME = 'LABST'.
      LS_FCAT-COLTEXT   = 'Quantidade'.
      LS_FCAT-SCRTEXT_L = 'Quantidade'.
      LS_FCAT-SCRTEXT_M = 'Quantidade'.
      LS_FCAT-SCRTEXT_S = 'Quantidade'.
    endif.

    if LS_FCAT-FIELDNAME = 'DESCR'.
      LS_FCAT-COLTEXT   = 'Descrição'.
      LS_FCAT-SCRTEXT_L = 'Descrição'.
      LS_FCAT-SCRTEXT_M = 'Descrição'.
      LS_FCAT-SCRTEXT_S = 'Descrição'.
      LS_FCAT-INTLEN = '000030'.
      LS_FCAT-DOMNAME = 'CHAR30'.
      LS_FCAT-EDIT = ''.
      LS_FCAT-CHECKTABLE = ''.
    endif.

    if LS_FCAT-FIELDNAME = 'NAME1'.
      LS_FCAT-COLTEXT   = 'Nome'.
      LS_FCAT-SCRTEXT_L = 'Nome'.
      LS_FCAT-SCRTEXT_M = 'Nome'.
      LS_FCAT-SCRTEXT_S = 'Nome'.
      LS_FCAT-INTLEN = '000040'.
      LS_FCAT-DOMNAME = 'CHAR40'.
      LS_FCAT-EDIT = ''.
      LS_FCAT-CHECKTABLE = ''.
    endif.

    case LS_FCAT-FIELDNAME.
      when 'MATNR'.
        LS_FCAT-COL_POS = 1.
        LS_FCAT-EDIT = 'X'.
      when 'DESCR'.
        LS_FCAT-COL_POS = 2.
*-CS2022000423-#77010-02.05.2022-JT-inicio
      when 'EQUNR'.
        LS_FCAT-COL_POS = 3.
        LS_FCAT-OUTPUTLEN = 18.
        LS_FCAT-EDIT = 'X'.
      when 'EQKTX'.
        LS_FCAT-COL_POS = 4.
        LS_FCAT-OUTPUTLEN = 40.
        LS_FCAT-EDIT = ''.
*-CS2022000423-#77010-02.05.2022-JT-fim
      when 'WERKS'.
        LS_FCAT-COL_POS = 5.
        LS_FCAT-OUTPUTLEN = 5.
        LS_FCAT-AUTO_VALUE = 'X'.
        LS_FCAT-EDIT = ''.
      when  'LABST'.
        LS_FCAT-COL_POS = 6.
        LS_FCAT-EDIT = ' '.
      when  'PERNR'.
        LS_FCAT-COL_POS = 7.
        LS_FCAT-EDIT = ''.
      when  'NAME1'.
        LS_FCAT-COL_POS = 8.
      when  'BUDAT'.
        LS_FCAT-COL_POS = 9.
        LS_FCAT-EDIT = ''.
    endcase.
    modify PT_FIELDCAT from LS_FCAT.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*      <--P_GT_FIELDCAT  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
form CREATE_AND_INIT_ALV changing PT_OUTTAB like GT_OUTTAB[]
                                  PT_FIELDCAT type LVC_T_FCAT.

  data: LT_EXCLUDE type UI_FUNCTIONS.

  create object G_CUSTOM_CONTAINER
    exporting
      CONTAINER_NAME = G_CONTAINER.
  create object G_GRID
    exporting
      I_PARENT = G_CUSTOM_CONTAINER.

  create object G_VERIFIER.
  set handler G_VERIFIER->HANDLE_DATA_CHANGED for G_GRID.


  perform BUILD_FIELDCAT changing PT_FIELDCAT.
  perform EXCLUDE_TB_FUNCTIONS changing LT_EXCLUDE.

  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*  gs_layout-cwidth_opt = 'X'.

  set handler  HANDLE_EVENT=>HANDLE_TOOLBAR for G_GRID.
  set handler  HANDLE_EVENT=>HANDLE_USER_COMMAND for G_GRID.

  call method G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    exporting
      IT_TOOLBAR_EXCLUDING = LT_EXCLUDE
      IS_LAYOUT            = GS_LAYOUT
    changing
      IT_FIELDCATALOG      = PT_FIELDCAT
      IT_OUTTAB            = PT_OUTTAB[].

* Set editable cells to ready for input initially
  call method G_GRID->SET_READY_FOR_INPUT
    exporting
      I_READY_FOR_INPUT = 1.

* Registering the EDIT Event

  call method G_GRID->REGISTER_EDIT_EVENT
    exporting
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER
    exceptions
      ERROR      = 1
      others     = 2.

  call method G_GRID->REGISTER_EDIT_EVENT
    exporting
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

endform.                               "CREATE_AND_INIT_ALV


*---------------------------------------------
form EXCLUDE_TB_FUNCTIONS changing PT_EXCLUDE type UI_FUNCTIONS.

  refresh PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_CHECK             to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_DETAIL            to PT_EXCLUDE. "Botão Detalhe
  append CL_GUI_ALV_GRID=>MC_FC_REFRESH           to PT_EXCLUDE. "Botão Refresh
  append CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           to PT_EXCLUDE. "Botão Recortar
  append CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         to PT_EXCLUDE. "Botão Colar com Sobregravação
  append CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW to PT_EXCLUDE. "Botão Colar em Nova Linha
  append CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      to PT_EXCLUDE. "Botão Duplicar Linha
  append CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    to PT_EXCLUDE. "Botão Anexar Linha
  append CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          to PT_EXCLUDE. "Botão Copiar Texto
  append CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          to PT_EXCLUDE. "Botão Anular
  append CL_GUI_ALV_GRID=>MC_FC_GRAPH             to PT_EXCLUDE. "Botão Grafico
  append CL_GUI_ALV_GRID=>MC_FC_INFO              to PT_EXCLUDE. "Botão Help
  append CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    to PT_EXCLUDE. "Botão Inserir Linha
  append CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    to PT_EXCLUDE. "Botão Deletar Linha
  append CL_GUI_ALV_GRID=>MC_FC_SEND              to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SEPARATOR         to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SORT              to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SORT_ASC          to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SORT_DSC          to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SUBTOT            to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_SUM               to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_TO_OFFICE         to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE       to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_UNFIX_COLUMNS     to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_URL_COPY_TO_CLIPBOARD   to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VARIANT_ADMIN     to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VIEWS             to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL      to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL        to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID         to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS        to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_LY_NO_INSERT_ROWS    to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_EXPORT            to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_FILTER            to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_PASTE             to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_SUBTOT            to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_SUM               to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_VARIANT           to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_MB_VIEW              to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_PRINT             to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_FIND              to PT_EXCLUDE.
  append CL_GUI_ALV_GRID=>MC_FC_FIND_MORE         to PT_EXCLUDE.

endform.                               " EXCLUDE_TB_FUNCTIONS
*------------------------------------------
form EFETUAR_LANCAMENTO.
  data: LS_HEADER  type ALM_ME_NOTIF_HEADER,
        LS_NOT_EXP type BAPI2080_NOTHDRE.

  data: LT_RET type BAPIRET2_TAB,
        LS_RET type BAPIRET2.

  data: LS_VIQMEL     type VIQMEL.

  data: EL_HEADER type BAPI2017_GM_HEAD_01,
        EL_CODE   type BAPI2017_GM_CODE,
        EL_RETURN type BAPIRET2.

  data: LV_TABIX type SY-TABIX.

  data: TL_ITEM   type table of BAPI2017_GM_ITEM_CREATE,
        TL_RETURN type table of BAPIRET2.

  data: LT_ZPMT0040 type table of ZPMT0040,
        LS_ZPMT0040 type ZPMT0040.

  data: LT_NOTIFICATION_PARTNER type table of  ALM_ME_PARTNER_KEY_STRUCT.
  data: LS_NOTIFICATION_PARTNER type ALM_ME_PARTNER_KEY_STRUCT.

  field-symbols:
    <FL_ITEM> type BAPI2017_GM_ITEM_CREATE.

  data: VL_MAT_DOC  type BAPI2017_GM_HEAD_RET-MAT_DOC,
        VL_DOC_YEAR type BAPI2017_GM_HEAD_RET-DOC_YEAR.

  refresh TL_ITEM.

  loop at GT_OUTTAB.
    if GT_OUTTAB-MATNR is initial or GT_OUTTAB-PROCESS is not initial.
      continue.
    endif.
    LV_TABIX = SY-TABIX.
    move-corresponding GT_OUTTAB to LS_ZPMT0040.

    clear: EL_HEADER,
           EL_CODE.
    EL_HEADER-PSTNG_DATE = GT_OUTTAB-BUDAT.
    EL_HEADER-DOC_DATE   = GT_OUTTAB-BUDAT.
    EL_HEADER-PR_UNAME   = SY-UNAME.
    EL_HEADER-HEADER_TXT = 'EMPRESTIMO DE FERRAMENTAS'.
    EL_CODE-GM_CODE      = '04'.

    unassign <FL_ITEM>.
    append initial line to TL_ITEM assigning <FL_ITEM>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
    "    MOVE: gt_outtab-matnr  TO <fl_item>-material,
    data(V_LEN2) = STRLEN( GT_OUTTAB-MATNR ).
    if V_LEN2 > 18.
      move GT_OUTTAB-MATNR to <FL_ITEM>-MATERIAL_LONG .
    else.
      move GT_OUTTAB-MATNR to <FL_ITEM>-MATERIAL .
    endif.
    move:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
              GT_OUTTAB-WERKS  to <FL_ITEM>-PLANT,
              'FER1'  to <FL_ITEM>-STGE_LOC,
              'OF01'  to <FL_ITEM>-MOVE_STLOC,
              GT_OUTTAB-LABST  to <FL_ITEM>-ENTRY_QNT.
*            gt_outtab-meins  TO <fl_item>-entry_uom.
**
    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        INPUT  = GT_OUTTAB-MATNR
      importing
        OUTPUT = GT_OUTTAB-MATNR.

*-CS2022000423-#77010-02.05.2022-JT-inicio
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = GT_OUTTAB-EQUNR
      importing
        OUTPUT = GT_OUTTAB-EQUNR.
*-CS2022000423-#77010-02.05.2022-JT-fim

    <FL_ITEM>-MOVE_TYPE = 'ZF3'.
    LS_ZPMT0040-DATA_CRI = SY-DATUM.
    LS_ZPMT0040-HORA_CRI = SY-UZEIT.
    LS_ZPMT0040-USUARIO_CRI = SY-UNAME.
    append LS_ZPMT0040 to LT_ZPMT0040.

  endloop.

  if LT_ZPMT0040 is not initial.
    sort LT_ZPMT0040 by MATNR EQUNR.
  endif.

  if TL_ITEM[] is not initial.
    set update task local.
    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      exporting
        GOODSMVT_HEADER  = EL_HEADER
        GOODSMVT_CODE    = EL_CODE
      importing
        MATERIALDOCUMENT = VL_MAT_DOC
        MATDOCUMENTYEAR  = VL_DOC_YEAR
      tables
        GOODSMVT_ITEM    = TL_ITEM
        RETURN           = TL_RETURN.

    read table TL_RETURN into EL_RETURN
                         with key TYPE = 'E'.
    if SY-SUBRC is not initial.
      loop at  LT_ZPMT0040 into LS_ZPMT0040.
        LS_ZPMT0040-MAT_DOC = VL_MAT_DOC.
        LS_ZPMT0040-DOC_YEAR = VL_DOC_YEAR.
        LS_ZPMT0040-DATA_CRI = SY-DATUM.
        LS_ZPMT0040-HORA_CRI = SY-UZEIT.
        LS_ZPMT0040-USUARIO_CRI = SY-UNAME.
        concatenate 'GOODSMVT :' VL_MAT_DOC '-' VL_DOC_YEAR into LS_ZPMT0040-OBSERV.
        modify LT_ZPMT0040 from LS_ZPMT0040.
      endloop.
      message S000(Z_MM) with 'Gerado :' LS_ZPMT0040-OBSERV.
      clear LS_ZPMT0040.

      loop at GT_OUTTAB.
        if GT_OUTTAB-MATNR is initial or GT_OUTTAB-PROCESS is not initial.
          continue.
        endif.
        LV_TABIX = SY-TABIX.

*-CS2022000423-#77010-02.05.2022-JT-inicio
*       ls_header-material  = gt_outtab-matnr.
        LS_HEADER-ASSEMBLY   = GT_OUTTAB-MATNR.
        LS_HEADER-EQUIPMENT  = GT_OUTTAB-EQUNR.
*-CS2022000423-#77010-02.05.2022-JT-fim

        LS_HEADER-SHORT_TEXT = 'EMPRESTIMO DE FERRAMENTAS'.
        LS_HEADER-CAT_TYPE   = 'D'.         " codificação
        LS_HEADER-CODE_GROUP = 'F0000060'.   "
        LS_HEADER-CODING     = '0010'.      "
        LS_HEADER-NOTIF_TYPE = 'FR'.
        LS_HEADER-REPORTEDBY = SY-UNAME.

        refresh LT_NOTIFICATION_PARTNER.
        LS_NOTIFICATION_PARTNER-PARTNER_ROLE = 'VW'.
        LS_NOTIFICATION_PARTNER-PARTNER_KEY = 'K5' &&  GT_OUTTAB-PERNR.
        append LS_NOTIFICATION_PARTNER to LT_NOTIFICATION_PARTNER.


        call function 'ALM_ME_NOTIFICATION_CREATE' "#EC CI_USAGE_OK[2438006]
          exporting
            NOTIFICATION_HEADER         = LS_HEADER
            NOTIF_TYPE                  = 'FR'
            I_PARTNER_TPA_KEY           = 'X'
          importing
            NOTIFICATION_EXPORT         = LS_NOT_EXP
          tables
            NOTIFICATION_PARTNER        = LT_NOTIFICATION_PARTNER
            RETURN                      = LT_RET
          exceptions
            ERROR_IN_INPUT_DATA         = 1
            NOTIFICATION_ALREADY_EXISTS = 2
            TASK_NOT_REL_OR_COMPL       = 3
            USER_STATUS_NOT_CHANGED     = 4
            others                      = 5.


        call function 'IQS4_SAVE_NOTIFICATION'
          exporting
            I_QMNUM  = LS_NOT_EXP-NOTIF_NO
            I_COMMIT = 'X'
          importing
            E_VIQMEL = LS_VIQMEL
          tables
            RETURN   = LT_RET.

        read table LT_RET into LS_RET with key TYPE = 'E'.
        if SY-SUBRC ne 0.
          GT_OUTTAB-PROCESS = 'X'.
          modify GT_OUTTAB from GT_OUTTAB index LV_TABIX.

          read table LT_ZPMT0040 into LS_ZPMT0040
          with key MATNR = GT_OUTTAB-MATNR
                   EQUNR = GT_OUTTAB-EQUNR
          binary search.
          if SY-SUBRC = 0.
            LS_ZPMT0040-QMNUM = LS_VIQMEL-QMNUM.
            LS_ZPMT0040-OBSERV = 'NOTA QM :' &&  LS_VIQMEL-QMNUM.
            LS_ZPMT0040-URL_DIGITAL_LEFT     =  GV_URL_DIGITAL_LEFT.
            LS_ZPMT0040-URL_DIGITAL_RIGHT    =  GV_URL_DIGITAL_RIGHT.

            modify LT_ZPMT0040 from LS_ZPMT0040 index SY-TABIX.

          endif.
        else.
          call function 'RSCRMBW_DISPLAY_BAPIRET2'
            tables
              IT_RETURN = LT_RET.
          GT_OUTTAB-PROCESS = ''.
          GT_OUTTAB-OBSERV = LS_RET-MESSAGE.
          modify GT_OUTTAB from GT_OUTTAB index LV_TABIX.
          read table LT_ZPMT0040 into LS_ZPMT0040
          with key MATNR = GT_OUTTAB-MATNR
                   EQUNR = GT_OUTTAB-EQUNR
          binary search.
          if SY-SUBRC = 0.
            LS_ZPMT0040-QMNUM = ''.
            LS_ZPMT0040-OBSERV = LS_RET-MESSAGE.
            modify LT_ZPMT0040 from LS_ZPMT0040 index SY-TABIX.
          endif.
        endif.

      endloop.
    else.
      call function 'RSCRMBW_DISPLAY_BAPIRET2'
        tables
          IT_RETURN = TL_RETURN.
      loop at  LT_ZPMT0040 into LS_ZPMT0040.
        concatenate LS_ZPMT0040-OBSERV '/' EL_RETURN-MESSAGE into LS_ZPMT0040-OBSERV.
        LS_ZPMT0040-DATA_CRI = SY-DATUM.
        LS_ZPMT0040-HORA_CRI = SY-UZEIT.
        LS_ZPMT0040-USUARIO_CRI = SY-UNAME.
        modify LT_ZPMT0040 from LS_ZPMT0040.
      endloop.
      loop at GT_OUTTAB .
        GT_OUTTAB-PROCESS = ''.
        concatenate GT_OUTTAB-OBSERV '/' EL_RETURN-MESSAGE into GT_OUTTAB-OBSERV.
        modify GT_OUTTAB from GT_OUTTAB.
      endloop.

    endif.
  endif.
  if LT_ZPMT0040[] is not initial.
    modify ZPMT0040 from table LT_ZPMT0040.
    commit work.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  Z_CALL_FORM                                              *
*&---------------------------------------------------------------------*
*                            Chama Formulário                          *
*----------------------------------------------------------------------*
form Z_CALL_FORM.

  data: VL_FORMNAME type TDSFNAME,
        VL_NAME     type RS38L_FNAM.

  data: LT_ZPMS0040 type table of ZPMS0040,
        LS_ZPMS0040 type ZPMS0040.

  data: LV_EMPREGADO(100) type C.
  data: LV_DT_ADMISSAO(100) type C.
  data: LV_UNIDADE(100) type C.
  data: LV_FUNCAO(100) type C.
  data: LV_SETOR(100) type C.

  data: LT_PA0001 type table of PA0001.
  data: LS_PA0001 type PA0001.

  loop at GT_OUTTAB.
    if GT_OUTTAB-PROCESS is initial.
      refresh LT_ZPMS0040.
      exit.
    endif.
    move-corresponding GT_OUTTAB to LS_ZPMS0040.
    append LS_ZPMS0040 to LT_ZPMS0040.
  endloop.

  concatenate GT_OUTTAB-PERNR  '-' GT_OUTTAB-NAME1 into LV_EMPREGADO separated by SPACE.

  if LT_ZPMS0040[] is not initial.
    VL_FORMNAME = 'ZPMF0006'.

    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        FORMNAME           = VL_FORMNAME
      importing
        FM_NAME            = VL_NAME
      exceptions
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        others             = 3.

    if SY-SUBRC <> 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
              with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      exit.
    endif.

    if GT_OUTTAB-PERNR is not initial.
      select * into table LT_PA0001
        from PA0001
        where PERNR = GT_OUTTAB-PERNR.

      sort  LT_PA0001 by BEGDA.
      read table LT_PA0001 into LS_PA0001 index 1.
      LV_DT_ADMISSAO = LS_PA0001-BEGDA+6(2)  && '/' &&  LS_PA0001-BEGDA+4(2) && '/' && LS_PA0001-BEGDA(4).

      sort  LT_PA0001 by ENDDA descending.
      read table LT_PA0001 into LS_PA0001 index 1.
      LV_EMPREGADO = LS_PA0001-ENAME.

      select single ORGTX into LV_SETOR
        from T527X
        where ORGEH = LS_PA0001-ORGEH
          and SPRSL = SY-LANGU
          and ENDDA > SY-DATUM.

      select single STLTX into LV_FUNCAO
        from T513S
        where STELL = LS_PA0001-STELL
          and SPRSL = SY-LANGU
          and ENDDA > SY-DATUM.

    endif.

    if GT_OUTTAB-WERKS is not initial.
      select single NAME1 into LV_UNIDADE
        from T001W
        where WERKS = GT_OUTTAB-WERKS.
      concatenate GT_OUTTAB-WERKS '-' LV_UNIDADE into LV_UNIDADE separated by SPACE.
    endif.
    call function VL_NAME
      exporting
        P_UNIDADE        = LV_UNIDADE
        P_EMPREGADO      = LV_EMPREGADO
        P_FUNCAO         = LV_FUNCAO
        P_SETOR          = LV_SETOR
        P_DT_ADMISSAO    = LV_DT_ADMISSAO
      tables
        T_EMPREST        = LT_ZPMS0040
      exceptions
        FORMATTING_ERROR = 1
        INTERNAL_ERROR   = 2
        SEND_ERROR       = 3
        USER_CANCELED    = 4
        others           = 5.

    if SY-SUBRC <> 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
              with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  else.
    if GT_OUTTAB[] is not initial.
      message S000(Z_MM) with 'Verifique erro :' GT_OUTTAB-OBSERV  display like 'E'.
    else.
      message S000(Z_MM) with 'Não existem informações a serem impressa.' display like 'E'.
    endif.
  endif.
endform.                    " Z_CALL_FORM

*&---------------------------------------------------------------------*
*&      Form  delete_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DELETE_ROWS .
  data: LS_ROW    type LVC_S_ROW.
  sort HANDLE_EVENT=>MT_SEL_ROWS by INDEX descending. " !!!
  loop at HANDLE_EVENT=>MT_SEL_ROWS into LS_ROW.
    delete GT_OUTTAB index LS_ROW-INDEX.
  endloop.
  if GT_OUTTAB[] is initial.
    loop at screen.
      if SCREEN-NAME = 'GV_WERKS' or
         SCREEN-NAME = 'GV_PERNR' or
         SCREEN-NAME = 'GV_BUDAT'.
        SCREEN-INPUT = 1.
        modify screen.
      endif.
    endloop.
  endif.
endform.                    " delete_rows

*&---------------------------------------------------------------------*
*&      Form  INSERT_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INSERT_ROW .
* define local data
  data: LD_VALUE1 type SPOP-VARVALUE1,
        LS_OUTTAB like line of GT_OUTTAB.

  if GV_WERKS is not initial and
     GV_PERNR is not initial and
     GV_NAME1 is not initial and
     GV_BUDAT is not initial .

    loop at screen.
      if SCREEN-NAME = 'GV_WERKS' or
         SCREEN-NAME = 'GV_PERNR' or
         SCREEN-NAME = 'GV_BUDAT'.
        SCREEN-INPUT = 0.
        modify screen.
      endif.
    endloop.

    LS_OUTTAB-WERKS = GV_WERKS.
    LS_OUTTAB-PERNR = GV_PERNR.
    LS_OUTTAB-NAME1 = GV_NAME1.
    LS_OUTTAB-BUDAT = GV_BUDAT.
    LS_OUTTAB-LABST = 1.
    append LS_OUTTAB to GT_OUTTAB.
  else.
    message S000(Z_MM) with 'Campos Centro,' 'Funcionário e Data Lçto' 'são obrigatório.' display like 'E'.
  endif.

endform.                    " INSERT_ROW

form Z_VALIDA_USUARIO.

  data: LS_GOOD type LVC_S_MODI,
        L_MATNR type MATNR,
        L_PERNR type ZPMT0040-PERNR,
        L_DESCR type MAKTX.

  data: L_NAME type PA0001-ENAME.

  data: LS_OUTTAB like line of GT_OUTTAB.

  data: L_FIREDATE type P0000-BEGDA.

  if GV_PERNR is not initial.
    call function 'RP_GET_FIRE_DATE'
      exporting
        PERSNR   = GV_PERNR
*       STATUS2  = '0'
      importing
        FIREDATE = L_FIREDATE.
    if L_FIREDATE is not initial.
      message S000(Z_MM) with  text-M01 display like 'E'.
      clear GV_NAME1.
      clear GV_PERNR.
    else.
      clear GV_NAME1.
      select single ENAME into GV_NAME1
        from PA0001
        where PERNR = GV_PERNR.
      if GV_NAME1 is initial.
        clear GV_PERNR.
        message S000(Z_MM) with text-M03 display like 'E'.
      endif.
    endif.
  else.
    clear GV_NAME1.
  endif.

  if GV_BUDAT is not initial and GV_BUDAT > SY-DATUM.
    clear GV_BUDAT.
    message S000(Z_MM) with text-M06 display like 'E'.
  endif.

  if GV_WERKS is not initial.
    select single WERKS into GV_WERKS
      from T001W
      where WERKS = GV_WERKS.
    if SY-SUBRC ne 0.
      clear GV_WERKS.
      message S000(Z_MM) with text-M08 display like 'E'.
    endif.
  endif.
endform.

form Z_NEW.

  refresh GT_OUTTAB.
  clear:  GV_WERKS,
          GV_BUDAT,
          GV_PERNR,
          GV_NAME1.

  clear: EG_ZPMT0044.
  clear: GV_DESC.

  if not G_CONTAINER is initial and SY-DYNNR ne '0300'.
    call method G_GRID->REFRESH_TABLE_DISPLAY
      exceptions
        FINISHED = 1
        others   = 2.
  endif.

endform.

form Z_VALIDA_MATERIAL.

  if EG_ZPMT0044-MATNR is not initial.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        INPUT  = EG_ZPMT0044-MATNR
      importing
        OUTPUT = EG_ZPMT0044-MATNR.

    select single MAKTX from MAKT into GV_DESC
                  where MATNR = EG_ZPMT0044-MATNR
                    and SPRAS = SY-LANGU.

    if SY-SUBRC ne 0.
      "CLEAR eg_zpmt0044-matnr. BUG - 78755 - CBRAND
      clear GV_DESC.

      message S000(Z_MM) with text-M02 display like 'E'.

    else.
      select single MATNR into EG_ZPMT0044-MATNR
          from MARA
          where MATNR = EG_ZPMT0044-MATNR
            and MTART = 'UNBW'.
      if SY-SUBRC ne 0.
        "CLEAR eg_zpmt0044-matnr. BUG - 78755 - CBRAND
        clear GV_DESC.

        message S000(Z_MM) with text-M04 display like 'E'.
      endif.
    endif.
  endif.

endform.

form Z_DESCARTE.

  data: EL_HEADER type BAPI2017_GM_HEAD_01,
        EL_CODE   type BAPI2017_GM_CODE,
        EL_RETURN type BAPIRET2.

  data: VL_MAT_DOC  type BAPI2017_GM_HEAD_RET-MAT_DOC,
        VL_DOC_YEAR type BAPI2017_GM_HEAD_RET-DOC_YEAR.

  data: TL_NOTIFITEM   type table of  BAPI2080_NOTITEMI,
        TL_NOTIFITEM_X type table of  BAPI2080_NOTITEMI_X,
        TL_NOTIFCAUS   type table of  BAPI2080_NOTCAUSI,
        TL_NOTIFCAUS_X type table of  BAPI2080_NOTCAUSI_X,
        TL_NOTIFACTV   type table of  BAPI2080_NOTACTVI,
        TL_NOTIFACTV_X type table of  BAPI2080_NOTACTVI_X,
        TL_NOTIFTASK   type table of  BAPI2080_NOTTASKI,
        TL_NOTIFTASK_X type table of  BAPI2080_NOTTASKI_X.

  data: EL_NOTIFHEADER   type BAPI2080_NOTHDRI,
        EL_NOTIFHEADER_X type  BAPI2080_NOTHDRI_X.

  data: EL_NOTIFHEADER_EXPORT type BAPI2080_NOTHDRE.
  data: EL_MAINTACTYTYPE_EXPORT type  ILA.

  data: EL_SYSTSTAT type BAPI2080_NOTSTI.

  data: TL_ITEM   type table of BAPI2017_GM_ITEM_CREATE,
        TL_RETURN type table of BAPIRET2.

  data: LIT_ZPMT0039 type table of ZPMT0039.

  data: LVA_DESCARTE type ZPMT0044-DESCARTE.

  field-symbols:
    <FL_ITEM> type BAPI2017_GM_ITEM_CREATE.

  data: begin of ITAB occurs 0,
          NAME(80) type C,
        end of ITAB.

  data: LVA_WMESSAGE_TEXT type BAPI_MSG,
        MSG_ALV           type CHAR80.

  if EG_ZPMT0044-MATNR  is not initial and
     EG_ZPMT0044-WERKS  is not initial and
     EG_ZPMT0044-OBSERV is not initial and
     EG_ZPMT0044-MATNR  is not initial and
     EG_ZPMT0044-BUDAT  is not initial and
     EG_ZPMT0044-EQUNR  is not initial and
     EG_ZPMT0044-LABST  is not initial.

*** PBI - 77012 - Inicio  CBRAND

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = EG_ZPMT0044-MATNR
      importing
        OUTPUT = EG_ZPMT0044-MATNR.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = EG_ZPMT0044-EQUNR
      importing
        OUTPUT = EG_ZPMT0044-EQUNR.


    select * into table LIT_ZPMT0039
     from ZPMT0039
     where MATNR = EG_ZPMT0044-MATNR
       and EQUNR = EG_ZPMT0044-EQUNR.

    if LIT_ZPMT0039 is not initial.

      perform VERIFICA_MATERIAL.

      if ( GIT_RESULTADO[] is not initial ).

        CL_DEMO_OUTPUT=>NEW(
          )->BEGIN_SECTION( `Erro ao processar descarte:`
          )->WRITE_TEXT( |Erro Material \n|
          )->WRITE_DATA( GIT_RESULTADO[]
          )->END_SECTION(
          )->DISPLAY( ).

      else.

        data: LWA_BDCDATA   type BDCDATA,
              LWA_CTUPARAMS type CTU_PARAMS,
              LIT_BDCDATA   type table of BDCDATA,
              LIT_MSG       type table of BDCMSGCOLL,
              LWA_ANSWER(1),
              GW_CHOICE     type SY-TABIX.

        clear LWA_BDCDATA.
        LWA_BDCDATA-PROGRAM  = 'ZPMR0009'.
        LWA_BDCDATA-DYNPRO   = '1000'.
        LWA_BDCDATA-DYNBEGIN = 'X'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_OKCODE'.
        LWA_BDCDATA-FVAL = '=BUSCAR'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_CURSOR'.
        LWA_BDCDATA-FVAL = 'GW_TELA-EQUNR'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TELA-EQUNR'.
        LWA_BDCDATA-FVAL = EG_ZPMT0044-EQUNR.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-PROGRAM  = 'ZPMR0009'.
        LWA_BDCDATA-DYNPRO   = '1000'.
        LWA_BDCDATA-DYNBEGIN = 'X'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_OKCODE'.
        LWA_BDCDATA-FVAL = '=ELIMINAR'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_CURSOR'.
        LWA_BDCDATA-FVAL = 'GW_TELA-EQUNR'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TELA-EQUNR'.
        LWA_BDCDATA-FVAL = EG_ZPMT0044-EQUNR.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-PROGRAM  = 'ZPMR0009'.
        LWA_BDCDATA-DYNPRO   = '3000'.
        LWA_BDCDATA-DYNBEGIN = 'X'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_OKCODE'.
        LWA_BDCDATA-FVAL = '=ELIMINAR'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'BDC_CURSOR'.
        LWA_BDCDATA-FVAL = 'GW_TL_ELIMINA-ARBPL'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TL_ELIMINA-CODE_ELIMINADOR'.
        LWA_BDCDATA-FVAL = '0060'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TL_ELIMINA-CODE'.
        LWA_BDCDATA-FVAL = '0110'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TL_ELIMINA-CODE_MOTIVO'.
        LWA_BDCDATA-FVAL = '0060'.
        append LWA_BDCDATA to LIT_BDCDATA.

        clear LWA_BDCDATA.
        LWA_BDCDATA-FNAM = 'GW_TL_ELIMINA-ARBPL'.
        LWA_BDCDATA-FVAL = 'FERRAMEN'.
        append LWA_BDCDATA to LIT_BDCDATA.


        LWA_CTUPARAMS-RACOMMIT = 'X'.
        LWA_CTUPARAMS-DISMODE  = 'N'." 'A'.
        LWA_CTUPARAMS-UPDMODE  = 'S'.

        call transaction 'ZPM0022' using LIT_BDCDATA options from LWA_CTUPARAMS
                                   messages into LIT_MSG.


        loop at LIT_MSG into data(LWA_MSG) where MSGTYP = 'E'.
          data(LWA_RESULTADO) = value TY_RESULTADO(
            ID      = LWA_MSG-MSGID
            NUMBER  = LWA_MSG-MSGNR
            MESSAGE = LWA_MSG-MSGV1 ).
          append LWA_RESULTADO to GIT_RESULTADO[].
          clear: LWA_RESULTADO.
        endloop.

        if GIT_RESULTADO[] is not initial.
          LWA_ANSWER = ' '.

          CL_DEMO_OUTPUT=>NEW(
          )->BEGIN_SECTION( `Erro ao processar descarte:`
          )->WRITE_TEXT( |Erro Material \n|
          )->WRITE_DATA( GIT_RESULTADO[]
          )->END_SECTION(
          )->DISPLAY( ).

        else.
          select single DESCARTE into LVA_DESCARTE
            from ZPMT0044
            where MATNR = EG_ZPMT0044-MATNR
              and EQUNR = EG_ZPMT0044-EQUNR.

          if LVA_DESCARTE is not initial.
            LWA_ANSWER = '0'.
            message S000(Z_MM) with 'Descarte a foi efetuado para' EG_ZPMT0044-MATNR '-' EG_ZPMT0044-EQUNR.
          else.
            LWA_ANSWER = '1'.
          endif.
        endif.

        check  LWA_ANSWER = '1'.

*** PBI - 77012 - Fim CBRAND
        clear: EL_HEADER,
               EL_CODE.

        EL_HEADER-PSTNG_DATE = SY-DATUM.
        EL_HEADER-DOC_DATE   = EG_ZPMT0044-BUDAT.
        EL_HEADER-PR_UNAME   = SY-UNAME.
        EL_HEADER-HEADER_TXT = EG_ZPMT0044-OBSERV.
        EL_CODE-GM_CODE      = '06'.

        refresh TL_ITEM.
        unassign <FL_ITEM>.
        append initial line to TL_ITEM assigning <FL_ITEM>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
        "        MOVE: eg_zpmt0044-matnr  TO <fl_item>-material,
        data(V_LEN1) = STRLEN( EG_ZPMT0044-MATNR ).
        if V_LEN1 > 18.
          move EG_ZPMT0044-MATNR to <FL_ITEM>-MATERIAL_LONG .
        else.
          move EG_ZPMT0044-MATNR to <FL_ITEM>-MATERIAL .
        endif.
        move:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                      EG_ZPMT0044-WERKS  to <FL_ITEM>-PLANT,
                        'FER1'  to <FL_ITEM>-STGE_LOC,
                        'FER1'  to <FL_ITEM>-MOVE_STLOC,
                      EG_ZPMT0044-LABST  to <FL_ITEM>-ENTRY_QNT.

        call function 'CONVERSION_EXIT_MATN1_INPUT'
          exporting
            INPUT  = EG_ZPMT0044-MATNR
          importing
            OUTPUT = EG_ZPMT0044-MATNR.

        move 'ZFB' to <FL_ITEM>-MOVE_TYPE.

        clear: VL_MAT_DOC,
               VL_DOC_YEAR.

        refresh TL_RETURN.

        set update task local.
        call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          exporting
            GOODSMVT_HEADER  = EL_HEADER
            GOODSMVT_CODE    = EL_CODE
          importing
            MATERIALDOCUMENT = VL_MAT_DOC
            MATDOCUMENTYEAR  = VL_DOC_YEAR
          tables
            GOODSMVT_ITEM    = TL_ITEM
            RETURN           = TL_RETURN.

        read table TL_RETURN into EL_RETURN
                             with key TYPE = 'E'.
        if SY-SUBRC is initial.

          call function 'RSCRMBW_DISPLAY_BAPIRET2'
            tables
              IT_RETURN = TL_RETURN.

        else.
          EG_ZPMT0044-MANDT       = SY-MANDT.
          EG_ZPMT0044-MAT_DOC     = VL_MAT_DOC.
          EG_ZPMT0044-DOC_YEAR    = VL_DOC_YEAR.
          EG_ZPMT0044-DATA_CRI    = SY-DATUM.
          EG_ZPMT0044-HORA_CRI    = SY-UZEIT.
          EG_ZPMT0044-USUARIO_CRI = SY-UNAME.
          EG_ZPMT0044-DESCARTE    = 'X'.

          insert ZPMT0044 from EG_ZPMT0044.
          commit work.
          message S000(Z_MM) with 'Descarte efetuado com sucesso. GOODS:' VL_MAT_DOC '-' VL_DOC_YEAR.
          perform Z_NEW.
        endif.
      endif.
    else.
      message S000(Z_MM) with text-M12 display like 'E'.
    endif.
  else.
    message S000(Z_MM) with text-M07 display like 'E'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form VERIFICA_MATERIAL.

  data: IT_RESULTADO  type table of TY_RESULTADO.

  data: LVA_EQUIPMENT type ZPMT0044-EQUNR,
        LWA_EQ_HEADER type ALM_ME_TOB_HEADER.

  data: LIT_RET type BAPIRET2_TAB,
        LWA_RET type BAPIRET2.

  data: LVA_EQUNR type ZPMT0044-EQUNR,
        LVA_MATNR type ZPMT0044-MATNR,
        LVA_ERRO  type CHAR1.

  clear: GIT_RESULTADO.


  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = EG_ZPMT0044-EQUNR
    importing
      OUTPUT = LVA_EQUNR.


  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = EG_ZPMT0044-MATNR
    importing
      OUTPUT = LVA_MATNR.


  call function 'ALM_ME_EQUIPMENT_GETDETAIL'
    exporting
      I_EQUIPMENT    = LVA_EQUNR
    importing
      E_EQUI_HEADER  = LWA_EQ_HEADER
    tables
      RETURN         = LIT_RET
    exceptions
      NOT_SUCCESSFUL = 1
      others         = 2.

  if SY-SUBRC = 0.
    if LWA_EQ_HEADER-SUBMT ne LVA_MATNR.
      data(LWA_RESULTADO) = value TY_RESULTADO(
      ID      = '00'
      NUMBER  = 'M11'
      MESSAGE = text-M11  ).
      append LWA_RESULTADO to GIT_RESULTADO[].
      clear: LWA_RESULTADO.
    endif.
  else.
    loop at LIT_RET into LWA_RET.
      LWA_RESULTADO = value TY_RESULTADO(
            ID      = LWA_RET-ID
            NUMBER  = LWA_RET-NUMBER
            MESSAGE = LWA_RET-MESSAGE ).
      append LWA_RESULTADO to GIT_RESULTADO[].
      clear: LWA_RESULTADO.
    endloop.
  endif.

  select QMNUM
    into table @data(T_0040)
    from ZPMT0040
   where WERKS = @EG_ZPMT0044-WERKS
     and MATNR = @LVA_MATNR
     and EQUNR = @LVA_EQUNR.

  select QMNUM
    into table @data(T_0041)
    from ZPMT0041
   where WERKS = @EG_ZPMT0044-WERKS
     and MATNR = @LVA_MATNR
     and EQUNR = @LVA_EQUNR.

  free: LVA_ERRO.

  loop at T_0040 into data(W_0040).
    read table T_0041 into data(W_0041) with key QMNUM = W_0040-QMNUM.
    if SY-SUBRC <> 0.
      LVA_ERRO = ABAP_TRUE.
      exit.
    endif.
  endloop.

  loop at T_0041 into W_0041.
    read table T_0040 into W_0040 with key QMNUM = W_0041-QMNUM.
    if SY-SUBRC <> 0.
      LVA_ERRO = ABAP_TRUE.
      exit.
    endif.
  endloop.

  if LVA_ERRO = ABAP_TRUE.
    LWA_RESULTADO = value TY_RESULTADO(
          ID      = '00'
          NUMBER  = 'M11'
          MESSAGE = text-M20  ).
    append LWA_RESULTADO to GIT_RESULTADO[].
    clear: LWA_RESULTADO.
  endif.

endform.
