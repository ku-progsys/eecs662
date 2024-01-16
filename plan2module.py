import csv

def write_module_md(week, accumstr):
  with open("_modules/week-{}.md".format(str(week).zfill(2)), "w") as f:
    header = """
---
title: Week {}
---
    """.format(str(week).zfill(2)).strip()
    body = "\n".join([header, "", accumstr])
    f.write(body)

with open('plan.csv', newline='') as csvfile:
  plan_csv = csv.reader(csvfile)
  skipped_header = False
  week = 1
  accumstr = ""

  for row in plan_csv:
    if not skipped_header:
      skipped_header = True
      continue

    curr_week = int(row[0])

    if curr_week is not week:
      write_module_md(week, accumstr)
      week = curr_week
      accumstr = ""

    date = row[1].split("-")
    date.reverse()
    formatted_date = " ".join(date)
    day = row[2]
    outstr = """
{} {}
: {}
  : []()
    """.format(day, formatted_date, "").strip()
    accumstr += (outstr + "\n\n")

# write final week
write_module_md(week, accumstr)
