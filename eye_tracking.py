#!/usr/bin/env python3
# code from https://qiita.com/mix_dvd/items/98feedc8c98bc7790b30


import cv2
import time
import shutil
asset = {
        "asset_root": "src",
        "eye": "eye.txt",
        "mouth": "mouth.txt"
        }

cascade_file_root = "haarcascades"
parts = [{
            "part": "eye",
            "cascade_file": "haarcascade_eye.xml"},
         {
             "part": "eye",
             "cascade_file": "haarcascade_lefteye_2splits.xml"},
         {
             "part": "mouth",
             "cascade_file": ""}
         ]



if __name__ == '__main__':
    ESC_KEY = 27
    INTERVAL = 33
    FRAME_RATE = 30
    TERM_SIZE = shutil.get_terminal_size()

    ORG_WINDOW_NAME = "org"

    DEVICE_ID = 0

    # add cascade object to each dict in parts
    parts = [part.update({"cascade":
                         cv2.CascadeClassifier(f'{cascade_file_root}/{part.cascade_file}')})
             for part in parts]

    cap = cv2.VideoCapture(DEVICE_ID)

    end_flag, c_frame = cap.read()
    height, width, channels = c_frame.shape

    cv2.namedWindow(ORG_WINDOW_NAME)

    while end_flag == True:

        img = c_frame
        img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

        cv2.imshow(ORG_WINDOW_NAME, c_frame)
        for  part in parts:
            face_list = part.cascade.detectMultiScale(img_gray, minSize=(100, 100))

            # x: left > right
            # y: bottom > top
            for (x, y, w, h) in face_list:
                x_percent = round(x / width * 100)
                y_percent = round(y / height * 100)
                os.system(f'bash ./draw.sh {x_percent} {y_percent} {asset["asset_root"]}/{asset[part.part]}')
                time.sleep(0.5)
        key = cv2.waitKey(INTERVAL)
        if key == ESC_KEY:
            break

        end_flag, c_frame = cap.read()

    cv2.destroyAllWindows()
    cap.release()
