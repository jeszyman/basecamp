
# - Bash non-interactive emacs function

source config/${HOSTNAME}.sh

org_tables=()
org_tables=("libraries"
            "participants")

for table in "${org_tables[@]}";
do
  /usr/bin/emacs-snapshot --batch "${repo}/mpnst.org" -l "${repo}/src/org_tbl_export.el" --eval '(my-tbl-export "'"$table"'")'
done

/usr/bin/emacs-snapshot --batch "${repo}/mpnst.org" -l "${repo}/src/org_master_input_tbl_export.el" --eval '(my-tbl-export "'"input"'")'


# - Bash to make the csvs

org_tables=()
org_tables=("experiments"
          "cohorts"
          "subjects"
          "specimens"
          "isolates"
          "seq_runs"
          "libraries"
          "tarbase_targeting_gja1"
          "lit_targeting_gja1"
          "mirdb_targeting_human_gja1"
          "mirdb_targeting_mouse_gja1"
          "notch_rentschler")

for table in "${org_tables[@]}";
do
  /usr/bin/emacs-snapshot --batch "/home/jeszyman/repos/card-rad-bio/card-rad-bio.org" -l "/home/jeszyman/repos/card-rad-bio/src/org_tbl_export.el" --eval '(my-tbl-export "'"$table"'")'
done

/usr/bin/emacs-snapshot --batch "/home/jeszyman/repos/card-rad-bio/card-rad-bio.org" -l "/home/jeszyman/repos/card-rad-bio/src/org_input_tbl_export.el" --eval '(my-tbl-export "'"input"'")'
