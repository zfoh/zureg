import csv

names = []
with open('2023-06-07-badges.csv') as f:
    for row in csv.reader(f):
        names.append(row[3])
names = names[1:]  # Avoid 'Name' header
names = sorted(names, key=lambda s: s.lower())

pages = []
while names:
    pages.append(names[0:21])
    names = names[21:]

print('''
<!doctype HTML>
<html>
    <head>
        <style type="text/css" media="print">
            @page {
                size: auto;
                margin: 0mm;
            }
        </style>
        <style type="text/css">
            :root {
                --badge-width: 70mm;
                --badge-height: 42.4mm;
                --badge-margin-top: 1.0cm;
                --badge-margin-side: 0.5cm;
            }

            body {
                font-size: 0.5cm;
                font-family: sans;
                font-stretch: condensed;
                font-weight: bold;

                margin: 0px;
                padding: 0px;
            }

            .page {
                page-break-after: always;
                display: flex;
                flex-wrap: wrap;
            }

            .badge {
                break-inside: avoid;

                width: calc(var(--badge-width) - 2 * var(--badge-margin-side));
                height: calc(var(--badge-height) - var(--badge-margin-top));

                padding-top: var(--badge-margin-top);
                padding-left: var(--badge-margin-side);
                padding-right: var(--badge-margin-side);

                text-align: center;

                print-color-adjust: exact !important;
            }
        </style>
    </head>
    <body>
''')

for page in pages:
    print('<div class="page">')
    for name in page:
        print('<div class="badge">')
        print(name)
        print('<br><img style="width: 3cm; margin: 0.4cm" src="./badges.svg">')
        print('</div>')
    print('</div>')

print('''
    </body>
</html>
''')
