import json
import csv

def convert_translation_csv_to_json(prefix):

    all_trans = {
        "languages": ["en", "jp", "zh-cn", "zh-tw", "es"],
        "translation": [],
    }

    with open(f'translation_{prefix}.csv') as csv_file:
        reader = csv.DictReader(csv_file)
        for row in reader:
            new_trans = {
                "en": row["English"],
                "jp": row["Japanese"],
                "zh-cn": row["Chinese_Simplified"],
                "zh-tw": row["Chinese_Traditional"],
                "es": row["Spanish"]}

            allready_added = False
            for _, trans in enumerate(all_trans['translation']):
                if trans['en'] == new_trans['en']:
                    allready_added = True
            if not allready_added:
                all_trans['translation'].append(new_trans)

            for i, _ in enumerate(row["English"].split("|")):
                try:
                    new_trans = {
                        "en": row["English"].split("|")[i],
                        "jp": row["Japanese"].split("|")[i],
                        "zh-cn": row["Chinese_Simplified"].split("|")[i],
                        "zh-tw": row["Chinese_Traditional"].split("|")[i],
                        "es": row["Spanish"].split("|")[i]}
                    
                    allready_added = False
                    for _, trans in enumerate(all_trans['translation']):
                        if trans['en'] == new_trans['en']:
                            allready_added = True
                    if not allready_added:
                        all_trans['translation'].append(new_trans)
                except:
                    print("following line has error:")
                    print(row)

    with open(f'translation_{prefix}.json', 'w') as json_file:
        json.dump(all_trans, json_file, ensure_ascii=False, indent=4, sort_keys=True, separators=(',', ': '))

if __name__ == '__main__':
    convert_translation_csv_to_json('header')
    convert_translation_csv_to_json('region')
    convert_translation_csv_to_json('variable')
