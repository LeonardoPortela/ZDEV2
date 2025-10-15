***INCLUDE RSPOOPT .
* Attributes of Spool System defined by SAP
* used as key for table TSPOPTIONS

constants: spopt_front_trace like tspoptions-spoption
              value 'FRONT_TRACE',         " Trace level for frontend printing

           spopt_front_codepage like tspoptions-spoption
              value 'FRONT_CODEPAGE',      " codepage for LPD

           spopt_front_codepage_def like tspoptions-value
              value '1100',                " codepage for LPD

           spopt_lprint     like tspoptions-spoption
              value 'LPRINT_DEST',         " RFC destination for access method F

           spopt_lprint_def like tspoptions-value
              value 'SAPGUI',

           spopt_lprint_num like tspoptions-spoption
              value 'LPRINT_NUM',          " Number range

           spopt_lprint_saplpd like tspoptions-spoption
              value 'SAPLPD',              " SAPLPD command for method F

           spopt_lprint_saplpd_def like tspoptions-value
              value 'SAPLPD\SAPLPD.EXE -I',

           spopt_lprint_exec like tspoptions-spoption
              value 'LPRINT_EXEC',         " Command for WS_EXECUTE

           spopt_auth       like tspoptions-spoption
              value 'AUTHORITY',           " Authority mode

           spopt_trans      like tspoptions-spoption
              value 'TRANSPORT',           " Transport system connection

           spopt_activation like tspoptions-spoption
              value 'LASTACTIVATION',      " Automatic job activation

           spopt_activationiv  like tspoptions-spoption
              value 'ACTIVATIONIV',        " Activation interval

           spopt_activationcnt like tspoptions-spoption
              value 'ACTIVATIONCNT',       " Number of activated jobs

           spopt_cleanup    like tspoptions-spoption
              value 'LASTCLEANUP',         " Automatic spool cleanup

           spopt_cleanupiv  like tspoptions-spoption
              value 'CLEANUPIV',           " Cleanup interval

           spopt_cleanupcnt like tspoptions-spoption
              value 'CLEANUPCNT',          " Number of deleted requests

           spopt_rescan     like tspoptions-spoption
              value 'LASTRESCAN',          " Last spool scan

           spopt_rescancnt like tspoptions-spoption
              value 'RESCANCNT',           " Number of unprocessed jobs

           spopt_redir      like tspoptions-spoption
              value 'LASTREDIR',           " Last redirection of jobs

           spopt_redircnt   like tspoptions-spoption
              value 'REDIRCNT',            " Number of deleted requests

           spopt_rsdsspti   like tspoptions-spoption
              value 'RSDSSPTI_SCAN',       " Number of deleted requests

           spopt_devkey     like tspoptions-spoption
              value 'DEVICEKEY',           " last used device db key

           spopt_upddev     like tspoptions-spoption
              value 'UPDDEV',              " Device update done

           spopt_upgvers like tspoptions-spoption
              value 'UPGRADE_VERSION',     " Automatic table upgrades

           spopt_suppress_pageinfo like tspoptions-spoption
              value 'SUPPRESS_PAGEINF',    " Suppress page breaks in SP01 display

           spopt_realwidth like tspoptions-spoption
              value 'REALWIDTH',           " width of spool list

           spopt_realheight like tspoptions-spoption
              value 'REALHEIGHT',          " height of spool list

           spopt_xmiloglevel like tspoptions-spoption
              value 'XMILOGLEVEL',         " XMI Log-Level fuer XOM und XSP

           spopt_poll_time like tspoptions-spoption
              value 'POLL_TIME',           " Polling interval for print data

           spopt_poll_num like tspoptions-spoption
              value 'POLL_NUM',            " Polling interval for print data

           spopt_sp01_reset_sel like tspoptions-spoption
              value 'SP01_RESSEL',         " reset selection after action

           spopt_mail_prio like tspoptions-spoption
              value 'MAIL_PRIO',           " Mail priority for access method 'M'

           spopt_so_no_expire like tspoptions-spoption
              value 'SO_NO_EXPIRE',        " note 591430

           spopt_reactivate_frontendjobs like tspoptions-spoption
              value 'REACTIVATE_FRONT',    " Reactivate pending frontendjobs

           spopt_delete_web_frontendfile like tspoptions-spoption
              value 'DELETE_WEBFILE',      " Delete file for Webgui frontend print

           spopt_check_deletion type tspoptions-spoption
              value 'CHECK_DELETION',      " Display warning if processed request shall be deleted

           spopt_spcpc_allow_change type tspoptions-spoption
              value 'SPCPC_ALLOW_CHG',     " Allow changes for PAL printer

           spopt_spool_debug TYPE tspoptions-spoption
              VALUE 'SPOOL_DEBUG',

           spoopt_ads_pres_pdl TYPE tspoptions-spoption
              VALUE 'ADS_PRE_PDL',         " allow ADS printing on KYOCERA printers

           spoopt_ads_swin_pdl TYPE tspoptions-spoption
              VALUE 'ADS_SWIN_PDL',        " allow ADS printing on Windows printers

           spopt_use_spool_title TYPE tspoptions-spoption
              VALUE 'USE_SPOOL_TITLE',     " note 1101211

           spopt_show_part_list TYPE tspoptions-spoption
              VALUE 'SHOW_PART_LIST',      " show screen 1101 in SP01

           spopt_suppress_201 TYPE tspoptions-spoption
              VALUE 'SUPPRESS_201',

           spopt_append_spoolid TYPE tspoptions-spoption
              VALUE 'APPEND_SPOOLID',      " pass spoolid to kernel

           spopt_display_download_dialog TYPE tspoptions-spoption
              VALUE 'DOWNLOAD_DIALOG',     " display download dialog in RSPO_DOWNLOAD_SPOOLJOB

           spopt_mail_from_dialog TYPE tspoptions-spoption
              VALUE 'MAIL_FROM_DIALOG',    " display download dialog in RSPO_DOWNLOAD_SPOOLJOB

           spoopt_ads_rdif_pdl TYPE tspoptions-spoption
              VALUE 'ADS_RDIF_PDL',        " allow ADS printing on SAPGOF printers

           spoopt_ads_use_subdir TYPE tspoptions-spoption
              VALUE 'ADS_USE_SUBDIR',      " use subdirectories for ADS jobs

           spoopt_ads_use_kernel TYPE tspoptions-spoption
              VALUE 'ADS_USE_KERNEL',      " read/write ADS jobs in kernel

           spoopt_trc_upe TYPE tspoptions-spoption
              VALUE 'TRC_UPE',              " trace for UPE

           spoopt_html_fpr_enabled TYPE tspoptions-spoption
              VALUE 'HTML_FPR_ENABLED',     " push channel for browser based frontend print

           spoopt_single_call_download TYPE tspoptions-spoption
              VALUE 'ONE_CALL_DL'.      " do not call GUI_DOWNLOAD in a loop

* macros

DEFINE get_option.
call function 'RSPO_OPTION_GET'
    EXPORTING
      name  = &1
    IMPORTING
      value = &2.
END-OF-DEFINITION.

DEFINE flush.
  call function 'RSPO_OPTION_FLUSH'
    exporting
      name             = &1
    exceptions
      call_error       = 1
      operation_failed = 2
      others           = 3.
END-OF-DEFINITION.
