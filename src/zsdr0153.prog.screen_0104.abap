PROCESS BEFORE OUTPUT.
  MODULE d1215_modifizieren.
  MODULE pfstatus_d1215.
  "MODULE TITEL_SETZEN_D1215.
  MODULE t042z_lesen.
  MODULE tctrl_zahlwege_init.
  LOOP WITH CONTROL tctrl_zahlwege.
    MODULE d1215_modifizieren.
    FIELD gva_xesel.
    FIELD gva_ezsch.
    FIELD gva_eztxt.
    FIELD gva_xasel.
    FIELD gva_azsch.
    FIELD gva_aztxt.
    MODULE zahlweg_anzeigen.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP WITH CONTROL tctrl_zahlwege.
    FIELD gva_xesel.
    FIELD gva_ezsch.
    FIELD gva_eztxt.
    FIELD gva_xasel.
    FIELD gva_azsch.
    FIELD gva_aztxt.
    MODULE zahlweg_markieren.
    MODULE zahlweg_update.
  ENDLOOP.

  MODULE zahlweg_leiste.
  MODULE tctrl_zahlwege_blaettern.
  MODULE user_command_0104.
