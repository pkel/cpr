FROM python:3.9

RUN mkdir /rl
RUN mkdir /python
WORKDIR /rl
COPY rl /rl
COPY python /python
RUN pip install -r requirements.txt

